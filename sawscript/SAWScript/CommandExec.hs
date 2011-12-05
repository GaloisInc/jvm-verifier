{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module SAWScript.CommandExec(runProofs) where

-- Imports {{{1
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Int
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (Traversable, traverse)
import qualified Data.Vector as V
import Prelude hiding (catch)
import System.Directory (makeRelativeToCurrentDirectory)
import System.Exit
import System.FilePath
import System.IO (hFlush, stdout)
import Text.PrettyPrint.HughesPJ

import qualified Execution.Codebase as JSS
import SAWScript.ErrorPlus
import SAWScript.Utils
import qualified SAWScript.MethodAST as AST
import qualified SAWScript.MethodSpec as TC
import qualified SAWScript.TypeChecker as TC
import qualified SBVModel.SBV as SBV
import qualified SBVParser as SBV

import Verinf.Symbolic
import Verinf.Symbolic.Common(opDefIndex)
import Verinf.Utils.IOStateT

-- Executor primitives {{{1

data ExecutorState = ES {
    opCache :: OpCache
    -- | Java codebase
  , codebase :: JSS.Codebase
  , execOptions :: SSOpts
    -- | Maps SAWScript function names to corresponding operator definition.
  , sawOpMap :: Map String OpDef
    -- | Map from names to constant value bound to name.
  , globalLetBindings :: Map String (CValue,DagType)
    -- | Maps file paths to verifier commands.
  , parsedFiles :: Map FilePath [AST.Input AST.SAWScriptCommand]
    -- | Flag that indicates if verification commands should be executed.
  , runVerification :: Bool
    -- | Maps rule and let binding names to location where it was introduced.
    -- Contains names in sawOpMap
  , definedNames :: Map String Pos
    -- | Maps function names read in from SBV file into corresponding
    -- operator definition and position where operator was introduced.
  , sbvOpMap :: Map String (Pos,OpDef)
    -- | List of verification tasks found (in reverse order).
    -- Boolean stores if method should be validated.
  , verifications :: [(Bool,TC.VerifyParams)]
    -- | Maps rule names to corresponding rule.
  , rules :: Map String Rule
    -- | Set of currently enabled rules.
  , enabledRules :: Set String
  }

type ExecErrors = [(Pos,Doc,String)]

type Executor   = ErrorT ExecErrors (StateT ExecutorState IO)

type Evaluator = ErrorPlusT ExecErrors (StateT ExecutorState IO)

traverse_ :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f ()
traverse_ f d = () <$ traverse f d

exec :: Executor a -> Evaluator a
exec m = ErrorPlusT (runErrorT m)

eval :: Evaluator a -> Executor a
eval m = ErrorT (runErrorPlusT m)

-- | Record a non-fatal error.
reportError :: Pos -> Doc -> Executor ()
reportError pos msg = throwError [(pos,msg,"")]

-- | Given a file path, this returns the verifier commands in the file,
-- or throws an exception.
typecheckPath :: FilePath -> Executor ()
typecheckPath path = do
  m <- gets parsedFiles
  case Map.lookup path m of
    Nothing -> error $ "internal: Could not find file " ++ path
    Just cmds -> do
      eval $ traverse_ (\(AST.Input p c) -> tcCommand p c) cmds

getGlobalBindings :: Executor TC.GlobalBindings
getGlobalBindings = do
  oc <- gets opCache
  cb <- gets codebase
  opts <- gets execOptions
  opBindings <- gets sawOpMap
  constBindings <- gets globalLetBindings
  return $ TC.GlobalBindings { TC.opCache = oc
                             , TC.codeBase = cb
                             , TC.ssOpts = opts
                             , TC.opBindings
                             , TC.constBindings
                             }

enabledOpDefs :: ExecutorState -> Set OpIndex
enabledOpDefs s
  = Set.fromList
  $ map (opDefIndex . snd)
  $ filter ((`Set.member` enabledRules s) . fst)
  $ Map.toList
  $ sawOpMap s

-- | Write debug message to standard IO.
debugWrite :: String -> Executor ()
debugWrite msg = do
  verb <- gets (verbose . execOptions)
  when (verb >= 6) $ liftIO $ putStrLn msg

-- Rule functions {{{2

-- | Throw exception indicating a rule already exists.
recordName :: Pos -> String -> Executor ()
recordName pos name = do
  s <- get
  case Map.lookup name (definedNames s) of
    Nothing -> do
      put $! s { definedNames = Map.insert name pos (definedNames s) }
    Just absPos -> do
      relPos <- liftIO $ posRelativeToCurrentDirectory absPos
      reportError pos $ ftext $ 
        show name ++ " is already defined at " ++ show relPos ++ "."

-- | Record rule if a rule with that name does not already exist.
recordRule :: String -> Rule -> Bool -> Executor ()
recordRule nm rl enabled = do
  modify $ \s -> s { rules = Map.insert nm rl (rules s) }
  when enabled $
    modify $ \s -> s { enabledRules = Set.insert nm (enabledRules s) }

-- | Execute action if name is undefined, otherwise issue a non-fatal error.
checkNameIsDefined :: Pos -> String -> Executor ()
checkNameIsDefined pos name = do
  m <- gets rules
  unless (Map.member name m) $
    reportError pos $ ftext (show name ++ "is undefined.")

-- Operations for extracting DagType from AST expression types {{{1

-- | Returns argument types and result type.
opDefType :: OpDef -> (V.Vector DagType, DagType)
opDefType def = (opDefArgTypes def, opDefResultType def)

-- | Parse the FnType returned by the parser into symbolic dag types.
tcFnType :: OpCache -> AST.FnType -> (V.Vector DagType, DagType)
tcFnType oc (AST.FnType args res) =
  (V.map (TC.tcType oc) (V.fromList args), TC.tcType oc res)

-- Operations used for SBV Parsing {{{1

-- | Check uninterpreted functions expected in SBV are already defined.
checkSBVUninterpretedFunctions :: Pos -> FilePath -> SBV.SBVPgm -> Executor ()
checkSBVUninterpretedFunctions pos absolutePath sbv = do
  relativePath <- liftIO $ makeRelativeToCurrentDirectory absolutePath
  let SBV.SBVPgm (_ver, _, _cmds, _vc, _warn, sbvUninterpFns) = sbv
  oc <- gets opCache
  curSbvOps <- gets sbvOpMap
  forM_ sbvUninterpFns $ \((name, _loc), irType, _) -> do
    case Map.lookup name curSbvOps of
      Nothing -> do
        let msg = show relativePath ++ "contains the undefined function "
                  ++ show name ++ "."
        throwIOExecException pos (ftext msg) ""
      Just (_,def) -> do
        let resType = SBV.inferFunctionType oc irType
        unless (opDefType def == resType) $ do
          let msg = show name ++ " has an unexpected type in "
                      ++ show relativePath ++ "."
          throwIOExecException pos (ftext msg) ""

-- | Throw ExecException if SBVException is thrown.
throwSBVParseError :: Pos -> FilePath -> SBV.SBVException -> IO a
throwSBVParseError pos absolutePath e = do
  relativePath <- makeRelativeToCurrentDirectory absolutePath
  let msg = ftext ((show relativePath) ++ " could not be parsed:\n") $$
            text (SBV.ppSBVException e)
   in throwIOExecException pos msg ""

-- Verifier command execution {{{1

-- | Run method spec validation command.
runMethodSpecValidate :: Int -> Bool -> TC.VerifyParams -> IO ()
runMethodSpecValidate verb v vp = do
  let ir = TC.vpSpec vp
  let specName = show (TC.specName ir)
  let putPrefixTS msg = do
        ts <- getTimeStamp
        putStr $ "[" ++ ts ++ "] " ++ msg
        hFlush stdout
  let putPrefixTSLn msg = do
        ts <- getTimeStamp
        putStrLn $ "[" ++ ts ++ "] " ++ msg
  let writeAssumeCorrect = 
        putPrefixTSLn $ "Assuming " ++ specName ++ " is correct."
  let runValidate = do
        ((), elapsedTime) <- timeIt $ TC.validateMethodSpec vp
        let timeMsg = " [Time: " ++ elapsedTime ++ "]"
        when (verb == 1 ) $ 
          putStrLn $ "Done." ++ timeMsg
        when (verb > 1) $
          putStrLn $ "Finished " ++ specName ++ "." ++ timeMsg
  case TC.specValidationPlan ir of
    _ | not v -> writeAssumeCorrect
    TC.Skip -> writeAssumeCorrect
    TC.QuickCheck n _ -> do
      when (verb == 1) $ let t 1 = " test"; t _ = " tests" in
        putPrefixTS $ "Testing " ++ specName ++ " (" ++ show n ++ t n ++ ")... "
      when (verb > 1) $
        putPrefixTSLn $ "Start testing " ++ specName ++ "."
      runValidate
    TC.Verify{} -> do
      when (verb == 1) $
        putPrefixTS $ "Verifying  " ++ specName ++ "... "
      when (verb > 1) $
        putPrefixTSLn $ "Start verifying " ++ specName ++ "."
      runValidate

recordSBVOp :: Pos -> String -> OpDef -> Executor ()
recordSBVOp pos sbvOpName op = do
  s <- get
  case Map.lookup sbvOpName (sbvOpMap s) of
    Nothing -> 
      put s { sbvOpMap = Map.insert sbvOpName (pos,op) (sbvOpMap s) }
    Just (absPos,_) -> do
      relPos <- liftIO $ posRelativeToCurrentDirectory absPos
      reportError pos $ ftext $
        "The SBV function " ++ sbvOpName ++ " was previously defined at "
          ++ show relPos ++ "."

-- | Convert a logic term into a term.
logicTermToTerm :: TC.LogicExpr -> Term
logicTermToTerm (TC.Apply op args) = appTerm op (map logicTermToTerm args)
logicTermToTerm (TC.Cns cns tp) = mkConst cns tp
logicTermToTerm (TC.JavaValue _ _ _) =
  error "internal: Java value given to logicTermToTerm"
logicTermToTerm (TC.Var name tp) = mkVar name tp

tcVarBindings :: [AST.Input AST.VarBinding]
              -> Executor (Map String TC.LogicExpr)
tcVarBindings bindings = do
  -- Check for duplicates in bindings.
  let duplicates = Map.toList
                 $ Map.filter (\v -> length v > 1)
                 $ Map.fromListWith (++)
                 $ map (\(AST.Input pos (nm, _)) -> (nm, [pos]))
                 $ bindings
  unless (null duplicates) $ eval $
     let throwAlreadyDefined _ [] = error "internal: tcVarBindings"
         throwAlreadyDefined nm (prevAbsPos:r) = exec $ do
           pp <- liftIO $ posRelativeToCurrentDirectory prevAbsPos
           let msg = ftext $ show nm ++ "already defined at " ++ show pp ++ "."
           throwError (map (\pos -> (pos,msg,"")) r)
      in traverse_ (uncurry throwAlreadyDefined) duplicates
  -- Return map
  oc <- gets opCache
  let parseFn (AST.Input _ (nm, typeAst)) =
        let tp = TC.tcType oc typeAst
         in (nm, TC.Var nm tp)
  return $ Map.fromList $ map parseFn bindings

-- | Execute command
tcCommand :: Pos -> AST.SAWScriptCommand -> Evaluator ()

-- Execute commands from file.
tcCommand _ (AST.ImportCommand path) = exec $ typecheckPath path

tcCommand pos (AST.ExternSBV nm absolutePath astFnType) = exec $ do
  -- Parse SBV file.
  (op, rhsTerm) <- do
    oc <- gets opCache
    debugWrite $ "Parsing SBV import for " ++ nm
    -- Load SBV file
    sbv <- liftIO $ SBV.loadSBV absolutePath
    -- Check uninterpreted functions are defined.
    checkSBVUninterpretedFunctions pos absolutePath sbv
    -- Define uninterpreted function map.
    curSbvOps <- gets sbvOpMap
    let uninterpFns :: String -> [DagType] -> Maybe Op
        uninterpFns name _ = (groundOp . snd) <$> Map.lookup name curSbvOps
    -- Parse SBV
    liftIO $ handle (throwSBVParseError pos absolutePath) $
      SBV.parseSBVOp oc uninterpFns nm sbv
  -- Get expected type.
  oc <- gets opCache
  let fnType = tcFnType oc astFnType
  -- Check that op type matches expected type.
  do unless (fnType == opDefFnType op) $ liftIO $ do
       -- Get relative path as Doc for error messages.
       relativePath <- makeRelativeToCurrentDirectory absolutePath
       let msg = show relativePath ++ " has a different type than expected."
        in throwIOExecException pos (ftext msg) ""
  -- Update state with op and rules.
  do recordName pos nm
     modify $ \s -> s { sawOpMap = Map.insert nm op (sawOpMap s) }
     let lhs = evalTerm $ appTerm (groundOp op) (V.toList lhsArgs)
         lhsArgs = V.map (\i -> mkVar (show i) (argTypes V.! i))
                 $ V.enumFromN 0 (V.length argTypes)
         (argTypes,_) = fnType
     let rhs = nodeToTermCtor (fmap show . termInputId) rhsTerm
     recordRule nm (Rule nm lhs rhs) False
  -- Record SBV op.
  do let sbvOpName = dropExtension (takeFileName absolutePath)
     recordSBVOp pos sbvOpName op
  -- Print debug log.
  debugWrite $ "Finished process extern SBV " ++ nm

tcCommand pos (AST.GlobalLet name rhsAst) = exec $ do
  debugWrite $ "Start defining let " ++ name
  valueExpr <- do
    bindings <- getGlobalBindings
    let config = TC.mkGlobalTCConfig bindings Map.empty
    liftIO $ TC.tcLogicExpr config rhsAst
  val <- liftIO $ TC.globalEval valueExpr
  let tp = TC.typeOfLogicExpr valueExpr
  -- Record name.
  recordName pos name
  -- Update state with op and rules.
  modify $ \s ->
    s { globalLetBindings = Map.insert name (val, tp) (globalLetBindings s) }
  debugWrite $ "Finished defining let " ++ name
tcCommand pos (AST.GlobalFn name args resType rhsAst) = exec $ do
  bindings <- getGlobalBindings --TODO: Fix this.
  nameTypeMap <- tcVarBindings args
  let config = TC.mkGlobalTCConfig bindings nameTypeMap
  rhsExpr <- liftIO $ TC.tcLogicExpr config rhsAst
  --TODO: Implement this.
  recordName pos name
  error "Not yet implemented: GlobalFn " pos name args resType rhsExpr
tcCommand pos (AST.SBVPragma nm sbvName) = exec $ do
  m <- gets sawOpMap
  case Map.lookup nm m of
    Nothing -> reportError pos $ ftext $ show nm ++ " is undefined."
    Just op -> recordSBVOp pos sbvName op
tcCommand _ (AST.SetVerification val) = exec $ do
  modify $ \s -> s { runVerification = val }
tcCommand pos (AST.DeclareMethodSpec methodId cmds) = exec $ do
  cb <- gets codebase
  let mName:revClasspath = reverse methodId
  when (null revClasspath) $
    throwIOExecException pos (ftext "Missing class in method declaration.") ""
  let jvmClassName = intercalate "/" $ reverse revClasspath
  -- Get class
  thisClass <- liftIO $ JSS.lookupClass cb jvmClassName
  -- Resolve method spec IR
  bindings <- getGlobalBindings
  ruleNames <- Map.keysSet <$> gets rules
  ir <- liftIO $
    TC.resolveMethodSpecIR bindings ruleNames pos thisClass mName cmds
  -- Create verification task.
  v <- gets runVerification
  oc <- gets opCache
  opts <- gets execOptions
  overrides <- gets (map (TC.vpSpec . snd) . verifications)
  allRules <- gets rules
  enRules <- gets enabledRules
  enOps <- gets enabledOpDefs
  let vp = TC.VerifyParams
             { TC.vpOpCache = oc
             , TC.vpCode = cb
             , TC.vpOpts = opts
             , TC.vpSpec = ir
             , TC.vpOver = overrides
             , TC.vpRules = Map.elems allRules
             , TC.vpEnabledRules = enRules
             , TC.vpEnabledOps = enOps
             }
  -- Add methodIR to state for use in later verifications.
  modify $ \s -> s { verifications = (v,vp) : verifications s }

tcCommand pos (AST.Rule ruleName params astLhsExpr astRhsExpr) = exec $ do
  debugWrite $ "Start defining rule " ++ ruleName
  bindings <- getGlobalBindings
  nameTypeMap <- tcVarBindings params
  let config = TC.mkGlobalTCConfig bindings nameTypeMap
  lhsExpr <- liftIO $ TC.tcLogicExpr config astLhsExpr
  rhsExpr <- liftIO $ TC.tcLogicExpr config astRhsExpr
  -- Check types are equivalence
  let lhsTp = TC.typeOfLogicExpr lhsExpr
      rhsTp = TC.typeOfLogicExpr rhsExpr
  unless (lhsTp == rhsTp) $ do
    let msg = "In the rule " ++ ruleName
                ++ ", the left hand and right hand sides of the rule have distinct types "
                ++ ppType lhsTp ++ " and " ++ ppType rhsTp ++ "."
        res = "Please ensure the left and right-hand side of the rule have equivalent types."
     in throwIOExecException pos (ftext msg) res
  -- Check all vars in quantifier are used in left-hand-side.
  let paramVars = Set.fromList $ map (fst . AST.inpVal) params
      lhsVars = TC.logicExprVarNames lhsExpr
  unless (lhsVars == paramVars) $ do
    let msg = "The left hand side of " ++ show ruleName
               ++ " must refer to all of the variables in the quantifier."
     in throwIOExecException pos (ftext msg) ""
  -- Parse lhsExpr and rhsExpr and add rule.
  let rl = Rule ruleName (evalTerm (logicTermToTerm lhsExpr))
                         (evalTerm (logicTermToTerm rhsExpr))
  recordName pos ruleName
  recordRule ruleName rl True
  debugWrite $ "Finished defining rule " ++ ruleName

tcCommand pos (AST.Disable name) = exec $ do
  checkNameIsDefined pos name
  modify $ \s -> s { enabledRules = Set.delete name (enabledRules s) }

tcCommand pos (AST.Enable name) = exec $ do
  checkNameIsDefined pos name
  modify $ \s -> s { enabledRules = Set.insert name (enabledRules s) }

printFailure :: [(Pos,Doc,String)] -> IO ()
printFailure l = do
  ts <- getTimeStamp
  putStrLn $ "\n[" ++ ts ++ "] Verification failed!"
  forM_ l $ \(absPos,errorMsg,resolution) -> do
    putStrLn ""
    putStrLn =<< show `fmap` posRelativeToCurrentDirectory absPos
    let rend = renderStyle style { lineLength = 100 }
    putStrLn $ rend $ nest 2 errorMsg
    when (resolution /= "") $ do
      putStrLn ""
      putStrLn $ rend $ nest 2 $ ftext resolution

-- | This is the entry point from the front-end
-- The implicit assumption is that you can either return back with an exitCode;
-- or never come back with a proper call to exitWith..
runProofs :: JSS.Codebase
          -> SSOpts
          -> AST.SSPgm
          -> IO ExitCode
runProofs cb ssOpts files = do
  oc <- mkOpCache
  let initialPath = entryPoint ssOpts
  let initState = ES {
          opCache = oc
        , codebase = cb
        , execOptions = ssOpts
        , parsedFiles = files
        , runVerification = True
        , definedNames  = Map.empty
        , sawOpMap      = Map.fromList [ ("aget", getArrayValueOpDef)
                                       , ("aset", setArrayValueOpDef)]
        , sbvOpMap      = Map.empty
        , verifications = []
        , rules         = Map.empty
        , enabledRules  = Set.empty
        , globalLetBindings = Map.empty
        }
      action = do
        typecheckPath initialPath
        gets (reverse . verifications)
  res <- 
    catch (evalStateT (runErrorT action) initState) $
          \(ExecException absPos errorMsg resolution) ->
             return $ Left [(absPos,errorMsg, resolution)]
  case res of
    Left errMsgs -> do
      printFailure errMsgs
      return $ ExitFailure (-1)
    Right l -> do
      mapM_ (uncurry (runMethodSpecValidate (verbose ssOpts))) l
      putStrLn "Verification complete!"
      return ExitSuccess
