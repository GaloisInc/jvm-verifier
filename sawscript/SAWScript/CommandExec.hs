{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module SAWScript.CommandExec(runProofs) where

-- Imports {{{1
import Control.Applicative
import Control.Exception (handle)
import Control.Monad
import Data.Int
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
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

type Executor   = ErrorCollectorT ExecErrors (StateT ExecutorState IO)

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
    Just cmds -> mapM_ (\(AST.Input p c) -> try (tcCommand p c)) cmds

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
checkSBVUninterpretedFunctions pos relativePath sbv = do
  let SBV.SBVPgm (_ver, _, _cmds, _vc, _warn, sbvUninterpFns) = sbv
  oc <- gets opCache
  curSbvOps <- gets sbvOpMap
  sequenceAll_ 
    [ case Map.lookup name curSbvOps of
        Nothing -> do
          let msg = show relativePath ++ "contains the undefined function "
                    ++ show name ++ "."
          reportError pos (ftext msg)
        Just (_,def) -> do
          let resType = SBV.inferFunctionType oc irType
          unless (opDefType def == resType) $ do
            let msg = show name ++ " has an unexpected type in "
                        ++ show relativePath ++ "."
            reportError pos (ftext msg)
    | ((name, _loc), irType, _) <- sbvUninterpFns
    ]

-- | Throw ExecException if SBVException is thrown.
mkSBVParseError :: Pos -> FilePath -> SBV.SBVException -> IO ExecErrors
mkSBVParseError pos absolutePath e = do
  relativePath <- makeRelativeToCurrentDirectory absolutePath
  let msg = ftext ((show relativePath) ++ " could not be parsed:\n") $$
            text (SBV.ppSBVException e)
  return [(pos, msg, "")]

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
logicTermToTerm (TC.IntLit i we@(widthConstant -> Just w)) =
  mkConst (mkCInt w i) (SymInt we)
logicTermToTerm (TC.IntLit i we) = error $
  "internal: logicToTerm given integer " ++ show i ++ " with non-constant type "
    ++ ppWidthExpr we "."
logicTermToTerm (TC.JavaValue _ _ _) =
  error "internal: Java value given to logicTermToTerm"
logicTermToTerm (TC.Var name tp) = mkVar name tp

tcVarBindings :: [AST.Input AST.VarBinding] -> Executor [(String,TC.LogicExpr)]
tcVarBindings bindings = do
  -- Check for duplicates in bindings.
  let duplicates = Map.toList
                 $ Map.filter (\v -> length v > 1)
                 $ Map.fromListWith (++)
                 $ map (\(AST.Input pos (nm, _)) -> (nm, [pos]))
                 $ bindings
  unless (null duplicates) $ do
     let recordAlreadyDefined _ [] = error "internal: tcVarBindings"
         recordAlreadyDefined nm (prevAbsPos:r) = do
           pp <- liftIO $ posRelativeToCurrentDirectory prevAbsPos
           let msg = ftext $ show nm ++ "already defined at " ++ show pp ++ "."
           try $ throwError (map (\pos -> (pos,msg,"")) r)
      in mapM_ (uncurry recordAlreadyDefined) duplicates
  -- Return map
  oc <- gets opCache
  let parseFn (AST.Input _ (nm, typeAst)) =
        let tp = TC.tcType oc typeAst
         in (nm, TC.Var nm tp)
  return $ map parseFn bindings

-- | Update state with op and rules.
recordOp :: Pos -> OpDef -> Executor ()
recordOp pos op = do
  let nm = opDefName op
  recordName pos nm
  modify $ \s -> s { sawOpMap = Map.insert nm op (sawOpMap s) }
  -- Add rule if there is a definition.
  case opDefDefinition op of
    Nothing -> return ()
    Just rhsTerm -> 
      let argTypes = opDefArgTypes op
          lhsArgs = V.generate (V.length argTypes) $ \i ->
                      mkVar (show i) (argTypes V.! i)
          lhs = evalTerm $ appTerm (groundOp op) (V.toList lhsArgs)
          rhs = nodeToTermCtor (fmap show . termInputId) rhsTerm
       in recordRule nm (Rule nm lhs rhs) False

-- | Execute command
tcCommand :: Pos -> AST.SAWScriptCommand -> Executor ()

-- Execute commands from file.
tcCommand _ (AST.ImportCommand path) = typecheckPath path

tcCommand pos (AST.ExternSBV nm absolutePath astFnType) = do
  oc <- gets opCache
  let expectedFnType@(argTypes,resType) = tcFnType oc astFnType
  let mkDefault = liftIO (uninterpretedOp oc nm argTypes resType)
  -- Create op, using a default uninterpreted Op if SBV fails for any reason.
  op <- flip mplus mkDefault $ do
    debugWrite $ "Parsing SBV import for " ++ nm
    -- Load SBV file
    sbv <- liftIO $ SBV.loadSBV absolutePath
    -- Get relative path for errors.
    relativePath <- liftIO $ makeRelativeToCurrentDirectory absolutePath
    -- Check for common errors.
    both -- Check uninterpreted functions are defined.
         (checkSBVUninterpretedFunctions pos relativePath sbv) 
         (do -- Check that SBV type matches user-defined type.
             let isOk = expectedFnType == SBV.parseSBVType oc sbv
             -- Force evaluation of isOk to catch potential parse error
             -- from parseSBVType.
             handleIO (mkSBVParseError pos absolutePath) $
               seq isOk (return ())
             unless isOk $ do
               -- Get relative path as Doc for error messages.
               let msg = show relativePath 
                     ++ " has a different type than expected."
               reportError pos (ftext msg))
    -- Define uninterpreted function map.
    curSbvOps <- gets sbvOpMap
    let uninterpFns :: String -> [DagType] -> Maybe Op
        uninterpFns name _ = (groundOp . snd) <$> Map.lookup name curSbvOps
    -- Create actual operator.
    handleIO (mkSBVParseError pos absolutePath) $ do
      defineOp oc nm argTypes resType $ \de _ inps ->
        SBV.evalSBV oc uninterpFns sbv de inps
  both (do -- Update state with op and rules.
           recordOp pos op)
       (do -- Record SBV op.
           let sbvOpName = dropExtension (takeFileName absolutePath)
           recordSBVOp pos sbvOpName op)
  -- Print debug log.
  debugWrite $ "Finished process extern SBV " ++ nm

tcCommand pos (AST.GlobalLet name rhsAst) = do
  debugWrite $ "Start defining let " ++ name
  valueExpr <- do
    bindings <- getGlobalBindings
    let cfg = TC.mkGlobalTCConfig bindings Map.empty
    liftIO $ TC.tcLogicExpr rhsAst cfg
  let ifn = error "internal: globalLet given non-ground expression"
  -- TODO: Figure out what happens if globalEval fails.
  -- This could be if an operator is uninterpreted due to SBV parsing failing.
  val <- liftIO $ TC.globalEval ifn evalTermSemantics valueExpr
  let tp = TC.typeOfLogicExpr valueExpr
  -- Record name.
  recordName pos name
  -- Update state with op and rules.
  modify $ \s ->
    s { globalLetBindings = Map.insert name (val, tp) (globalLetBindings s) }
  debugWrite $ "Finished defining let " ++ name
tcCommand pos (AST.GlobalFn nm argAsts resTypeAst rhsAst) = do
  oc <- gets opCache
  -- Typecheck types.
  args <- tcVarBindings argAsts
  let (argNames, argVars) = unzip args
  let argTypes = V.fromList (map TC.typeOfLogicExpr argVars)
  let resType = TC.tcType oc resTypeAst
  -- Define operator.
  gbindings <- getGlobalBindings
  let mkDefault = liftIO (uninterpretedOp oc nm argTypes resType)
  op <- flip mplus mkDefault $ do
    liftIO $ defineOp oc nm argTypes resType $ \de op inputs -> do
      -- Typecheck right-hand expression.
      let bindings = 
            gbindings {
                TC.opBindings = Map.insert nm (opDef op)
                                 (TC.opBindings gbindings)
              }
      let cfg = TC.mkGlobalTCConfig bindings (Map.fromList args)
      rhsExpr <- TC.tcLogicExpr rhsAst cfg
      -- Evaluate right-hand expression.
      let inputMap = Map.fromList $ argNames `zip` (V.toList inputs)
          inputFn name = let Just v = Map.lookup name inputMap in return v
      TC.globalEval inputFn (deTermSemantics de) rhsExpr
  -- Update state with op and rules.
  recordOp pos op
tcCommand pos (AST.SBVPragma nm sbvName) = do
  m <- gets sawOpMap
  case Map.lookup nm m of
    Nothing -> reportError pos $ ftext $ show nm ++ " is undefined."
    Just op -> recordSBVOp pos sbvName op
tcCommand _ (AST.SetVerification val) = do
  modify $ \s -> s { runVerification = val }
tcCommand pos (AST.DeclareMethodSpec methodId cmds) = do
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

tcCommand pos (AST.Rule ruleName params astLhsExpr astRhsExpr) = do
  debugWrite $ "Start defining rule " ++ ruleName
  bindings <- getGlobalBindings
  ruleVars <- tcVarBindings params
  let cfg = TC.mkGlobalTCConfig bindings (Map.fromList ruleVars)
  lhsExpr <- liftIO $ TC.tcLogicExpr astLhsExpr cfg
  rhsExpr <- liftIO $ TC.tcLogicExpr astRhsExpr cfg
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

tcCommand pos (AST.Disable name) = do
  checkNameIsDefined pos name
  modify $ \s -> s { enabledRules = Set.delete name (enabledRules s) }

tcCommand pos (AST.Enable name) = do
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
  (errList,res) <-
    handle (\(ExecException absPos errorMsg resolution) ->
              return ([(absPos,errorMsg, resolution)], False)) $ do
      (errList, ml) <- evalStateT (runErrorCollectorT action) initState
      case (errList,ml) of
        ([],Just l) -> do
          mapM_ (uncurry (runMethodSpecValidate (verbose ssOpts))) l
          return ([], null errList)
        _ -> return (errList, False)
  if res then do
    putStrLn "Verification complete!"
    return ExitSuccess
  else do
    printFailure errList
    return $ ExitFailure (-1)
