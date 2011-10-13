{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module SAWScript.CommandExec(runProofs) where

-- Imports {{{1
import Control.Exception
import Control.Monad
import Control.Monad.Identity
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
import SAWScript.Utils
import qualified SAWScript.MethodAST as AST
import qualified SAWScript.MethodSpec as TC
import qualified SAWScript.TypeChecker as TC
import qualified SBVModel.SBV as SBV
import qualified SBVParser as SBV
import qualified Simulation as JSS

import Verinf.Symbolic
import Verinf.Utils.IOStateT
import Verinf.Utils.LogMonad

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
  , parsedFiles :: Map FilePath [AST.VerifierCommand]
    -- | Flag that indicates if verification commands should be executed.
  , runVerification :: Bool
    -- | Maps rule and let binding names to location where it was introduced.
    -- Contains names in sawOpMap
  , definedNames :: Map String Pos
    -- | Maps function names read in from SBV file into corresponding
    -- operator definition and position where operator was introduced.
  , sbvOpMap :: Map String (Pos,OpDef)
    -- | List of method specs added to state.
  , methodSpecs :: [TC.MethodSpecIR]
    -- | Maps rule names to corresponding rule.
  , rules :: Map String Rule
    -- | Set of currently enabled rules.
  , enabledRules :: Set String
  }

newtype MExecutor m a = Ex (StateT ExecutorState m a)
  deriving ( CatchMIO
           , Functor
           , Monad
           , MonadIO
           , MonadState ExecutorState
           , MonadTrans
           )

type Executor = MExecutor IO

instance JSS.HasCodebase Executor where
  getCodebase = gets codebase

-- | Given a file path, this returns the verifier commands in the file,
-- or throws an exception.
parseFile :: FilePath -> Executor [AST.VerifierCommand]
parseFile path = do
  m <- gets parsedFiles
  case Map.lookup path m of
    Nothing -> error $ "internal: Could not find file " ++ path
    Just cmds -> return cmds

getGlobalBindings :: Executor TC.GlobalBindings
getGlobalBindings = do
  cb <- gets codebase
  opts <- gets execOptions
  opBindings <- gets sawOpMap
  constBindings <- gets globalLetBindings
  return $ TC.GlobalBindings { TC.codeBase = cb
                             , TC.ssOpts = opts
                             , TC.opBindings
                             , TC.constBindings
                             }

-- verbosity {{{2

instance LogMonad Executor where
  getVerbosity = fmap verbose $ gets execOptions
  setVerbosity v = modify $ \s -> s { execOptions = (execOptions s) { verbose = v } }

-- | Write messages to standard IO.
whenVerbosityWrite :: (Int -> Bool) -> String -> Executor ()
whenVerbosityWrite cond msg = whenVerbosity cond $ liftIO $ putStrLn msg

-- | Write messages to standard IO without printing a line.
whenVerbosityWriteNoLn :: (Int -> Bool) -> String -> Executor ()
whenVerbosityWriteNoLn cond msg =
  whenVerbosity cond $ liftIO $ do
    putStr msg
    hFlush stdout

-- | Write debug message to standard IO.
debugWrite :: String -> Executor ()
debugWrite = whenVerbosityWrite (>=6)

-- Rule functions {{{2

-- | Throw exception indicating a rule already exists.
checkNameIsUndefined :: Pos -> String -> Executor ()
checkNameIsUndefined pos name = do
  m <- gets definedNames
  case Map.lookup name m of
    Nothing -> return ()
    Just absPos -> do
      relPos <- liftIO $ posRelativeToCurrentDirectory absPos
      throwIOExecException pos
                           (ftext "The name " <+> quotes (text name)
                              <+> ftext "has already been defined at "
                              <+> text (show relPos) <> char '.')
                           ("Please ensure all names are distinct.")

checkNameIsDefined :: Pos -> String -> Executor ()
checkNameIsDefined pos name = do
  m <- gets rules
  unless (Map.member name m) $ do
    throwIOExecException pos
                         (text "No operator or rule named " <+> quotes (text name)
                           <+> text "has been defined.")
                         ("Please check that the name is correct.")


-- Operations for extracting DagType from AST expression types {{{1

-- | Returns argument types and result type.
opDefType :: OpDef -> (V.Vector DagType, DagType)
opDefType def = (opDefArgTypes def, opDefResultType def)

-- | Parse the FnType returned by the parser into symbolic dag types.
parseFnType :: AST.FnType -> Executor (V.Vector DagType, DagType)
parseFnType (AST.FnType args res) = do
  globalBindings <- getGlobalBindings
  oc <- gets opCache
  let config = TC.mkGlobalTCConfig oc globalBindings Map.empty
  lift $ do
    parsedArgs <- V.mapM (TC.tcType config) (V.fromList args)
    parsedRes <- TC.tcType config res
    return (parsedArgs, parsedRes)

-- Operations used for SBV Parsing {{{1

-- | Check uninterpreted functions expected in SBV are already defined.
checkSBVUninterpretedFunctions :: Pos -> Doc -> SBV.SBVPgm -> Executor ()
checkSBVUninterpretedFunctions pos relativePath sbv = do
  let SBV.SBVPgm (_ver, _, _cmds, _vc, _warn, sbvUninterpFns) = sbv
  oc <- gets opCache
  curSbvOps <- gets sbvOpMap
  forM_ sbvUninterpFns $ \((name, _loc), irType, _) -> do
    case Map.lookup name curSbvOps of
      Nothing -> do
        let msg = text "The extern SBV file"
                    <+> relativePath
                    <+> text "calls an undefined uninterpreted function named"
                    <+> quotes (text name)
                    <> char '.'
            res = "Please load this extern SBV file before attempting to load \'"
                    ++ name ++ "\'."
        throwIOExecException pos msg res
      Just (_,def) -> do
        let resType = SBV.inferFunctionType oc irType
        unless (opDefType def == resType) $ do
          let msg = text "The type of the uninterpreted function"
                     <+> quotes (text name)
                     <+> text "does not match the type expected in the extern SBV file"
                     <+> relativePath <> char '.'
              res = "Please check that the correct SBV files match the Cryptol source."
          throwIOExecException pos msg res

-- | Throw ExecException if SBVException is thrown.
throwSBVParseError :: MonadIO m => Pos -> Doc -> SBV.SBVException -> m a
throwSBVParseError pos relativePath e =
  let msg = text "An internal error occurred when loading the SBV"
              <+> relativePath <> colon $$
              text (SBV.ppSBVException e)
      res = "Please reconfirm that the SBV filename is a valid SBV file from Cryptol."
   in throwIOExecException pos msg res

-- Verifier command execution {{{1

-- | Execute command
execute :: AST.VerifierCommand -> Executor ()
-- Execute commands from file.
execute (AST.ImportCommand _pos path) = do
  mapM_ execute =<< parseFile path
execute (AST.ExternSBV pos nm absolutePath astFnType) = do
  oc <- gets opCache
  -- Get relative path as Doc for error messages.
  relativePath <- liftIO $ fmap (doubleQuotes . text) $
                    makeRelativeToCurrentDirectory absolutePath
  -- Get name of op in Cryptol from filename.
  let sbvOpName = dropExtension (takeFileName absolutePath)
  -- Get current uninterpreted function map.
  curSbvOps <- gets sbvOpMap
  -- Check if rule for operator definition is undefined.
  checkNameIsUndefined pos nm
  -- Check SBV Op name is undefined.
  case Map.lookup sbvOpName curSbvOps of
    Nothing -> return ()
    Just (absPos,_) -> do
      relPos <- liftIO $ posRelativeToCurrentDirectory absPos
      let msg = (text "The Cryptol function"
                  <+> text sbvOpName
                  <+> ftext "has already been defined at"
                  <+> text (show relPos)
                  <> char '.')
          res = "Please check that each exported function is only loaded once."
       in throwIOExecException pos msg res
  -- Load SBV file
  sbv <- liftIO $ SBV.loadSBV absolutePath
  --- Parse SBV type to add recordDefs as needed.
  debugWrite $ "Parsing SBV type for " ++ nm
  let SBV.SBVPgm (_ver, sbvExprType, _cmds, _vc, _warn, _uninterpFns) = sbv
  -- Check that op type matches expected type.
  debugWrite $ "Checking expected type matches inferred type for " ++ nm
  fnType <- parseFnType astFnType
  let inferredType = SBV.inferFunctionType oc sbvExprType
  unless (fnType == inferredType) $
    let msg = (ftext "The type of the function in the imported SBV file"
                 $$ relativePath
                 $$ ftext "differs from the type provided to the extern command.")
        res = "Please check that the function exported from Cryptol via SBV "
               ++ "matches the type in the SAWScript file."
     in throwIOExecException pos msg res
  -- Check uninterpreted functions are defined.
  debugWrite $ "Checking uninterpreted inputs for " ++ nm
  checkSBVUninterpretedFunctions pos relativePath sbv
  -- Define uninterpreted function map.
  let uninterpFns :: String -> [DagType] -> Maybe Op
      uninterpFns name _ = fmap (groundOp . snd) $ Map.lookup name curSbvOps
  -- Parse SBV file.
  debugWrite $ "Parsing SBV inport for " ++ nm
  (op, SBV.WEF opFn) <-
    flip catchMIO (throwSBVParseError pos relativePath) $ lift $
      SBV.parseSBVOp oc uninterpFns nm sbv
  -- Create rule for definition.
  debugWrite $ "Creating rule definition for for " ++ nm
  let (argTypes,_) = fnType
  let lhsArgs = V.map (\i -> mkVar (show i) (argTypes V.! i))
              $ V.enumFromN 0 (V.length argTypes)
  let lhs = evalTerm $ appTerm (groundOp op) (V.toList lhsArgs)
  rhs <- lift $ runSymbolic oc $ do
    inputVars <- V.mapM freshUninterpretedVar argTypes
    ts <- getTermSemantics
    return $ nodeToTermCtor (fmap show . termInputId)
           $ runIdentity
           $ opFn ts inputVars
  -- Update state with op and rules.
  modify $ \s -> s { sbvOpMap = Map.insert sbvOpName (pos,op) (sbvOpMap s)
                   , definedNames = Map.insert nm pos (definedNames s)
                   , sawOpMap = Map.insert nm op (sawOpMap s)
                   , rules = Map.insert nm (Rule nm lhs rhs) (rules s)
                   --, enabledRules = Set.insert nm (enabledRules s)
                   }
  debugWrite $ "Finished process extern SBV " ++ nm
execute (AST.GlobalLet pos name astExpr) = do
  debugWrite $ "Start defining let " ++ name
  checkNameIsUndefined pos name
  oc <- gets opCache
  valueExpr <- do
    bindings <- getGlobalBindings
    let config = TC.mkGlobalTCConfig oc bindings Map.empty
    lift $ TC.tcExpr config astExpr
  val <- lift $ TC.globalEval oc valueExpr
  let tp = TC.getTypeOfExpr valueExpr
  modify $ \s -> s { definedNames = Map.insert name pos (definedNames s)
                   , globalLetBindings = Map.insert name (val, tp) (globalLetBindings s) }
  debugWrite $ "Finished defining let " ++ name
execute (AST.SetVerification _pos val) = do
  modify $ \s -> s { runVerification = val }
execute (AST.DeclareMethodSpec pos methodId cmds) = do
  oc <- gets opCache
  let mName:revClasspath = reverse methodId
  when (null revClasspath) $
    throwIOExecException pos (ftext "Missing class in method declaration.") ""
  let jvmClassName = intercalate "/" $ reverse revClasspath
  -- Get class
  thisClass <- JSS.lookupClass jvmClassName
  -- Resolve method spec IR
  ir <- do
    bindings <- getGlobalBindings
    lift $ TC.resolveMethodSpecIR oc bindings pos thisClass mName cmds
  v <- gets runVerification
  ts <- getTimeStamp
  let tactics = TC.methodSpecVerificationTactics ir
  let specName = TC.methodSpecName ir
  whenVerbosityWriteNoLn (==1) $
   let vnm = case tactics of
               [AST.QuickCheck _ _] -> "Testing"
               [AST.Skip]           -> "Parsing"
               _                    -> "Verifying"
    in "[" ++ ts ++ "] " ++ vnm ++ " \"" ++ TC.methodSpecName ir ++ "\"... "
  if v && (TC.methodSpecVerificationTactics ir /= [AST.Skip])
    then do
      let vnm = case tactics of
                  [AST.QuickCheck _ _] -> "testing"
                  _                    -> "verification of"
      whenVerbosityWrite (>1) $
        "[" ++ ts ++ "] Starting " ++ vnm  ++ " \"" ++ specName ++ "\"."
      ((), elapsedTime) <- timeIt $ do
        cb <- gets codebase
        opts <- gets execOptions
        overrides <- gets methodSpecs
        allRules <- gets rules
        enRules <- gets enabledRules
        let activeRules = map (allRules Map.!) $ Set.toList enRules
        liftIO $ TC.verifyMethodSpec oc pos cb opts ir overrides activeRules

      whenVerbosityWrite (==1) $ "Done. [Time: " ++ elapsedTime ++ "]"
      whenVerbosityWrite (>1) $
        "Completed " ++ vnm ++ " \"" ++ specName ++ "\". [Time: " ++ elapsedTime ++ "]"
    else do
      whenVerbosityWrite (==1) $ "Skipped."
      whenVerbosityWrite (>1) $
        "Skipped verification of \"" ++ TC.methodSpecName ir ++ "\"."
  -- Add methodIR to state for use in later verifications.
  modify $ \s -> s { methodSpecs = ir : methodSpecs s }
execute (AST.Rule pos ruleName params astLhsExpr astRhsExpr) = do
  debugWrite $ "Start defining rule " ++ ruleName
  checkNameIsUndefined pos ruleName
  oc <- gets opCache
  bindings <- getGlobalBindings
  -- | Get map from variable names to typed expressions.
  nameTypeMap <- lift $ flip execStateT Map.empty $ do
    forM_ params $ \(fieldPos, fieldNm, astType) -> do
      m <- get
      case Map.lookup fieldNm m of
        Just _ ->
          let msg = "Rule contains multiple declarations of the variable \'"
                     ++ fieldNm ++ "\'."
           in throwIOExecException fieldPos (ftext msg) ""
        Nothing -> do
          let config = TC.mkGlobalTCConfig oc bindings Map.empty
          tp <- lift $ TC.tcType config astType
          modify $ Map.insert fieldNm (TC.Var fieldNm tp)
  let config = TC.mkGlobalTCConfig oc bindings nameTypeMap
  lhsExpr <- lift $ TC.tcExpr config astLhsExpr
  rhsExpr <- lift $ TC.tcExpr config astRhsExpr
  -- Check types are equivalence
  let lhsTp = TC.getTypeOfExpr lhsExpr
      rhsTp = TC.getTypeOfExpr rhsExpr
  unless (lhsTp == rhsTp) $ do
    let msg = "In the rule " ++ ruleName
                ++ ", the left hand and right hand sides of the rule have distinct types "
                ++ ppType lhsTp ++ " and " ++ ppType rhsTp ++ "."
        res = "Please ensure the left and right-hand side of the rule have equivalent types."
     in throwIOExecException pos (ftext msg) res
  -- Check all vars in quantifier are used.
  let paramVars = Set.fromList $ map (\(_,nm,_) -> nm) params
      lhsVars = TC.typedExprVarNames lhsExpr
  unless (lhsVars == paramVars) $ do
    let msg = "In the rule '" ++ ruleName ++ "', the left hand side term does"
               ++ " not refer to all of the variables in the quantifier."
        res = "Please ensure that all variables are referred to in the"
               ++ " left-hand side of the rule, ensure the right-hand side"
               ++ " does not refer to variables unbound in the left-hand side."
     in throwIOExecException pos (ftext msg) res
  -- TODO: Parse lhsExpr and rhsExpr and add rule.
  let mkRuleTerm :: TC.Expr -> Term
      mkRuleTerm (TC.Apply op args) = appTerm op (map mkRuleTerm args)
      mkRuleTerm (TC.Cns cns tp) = mkConst cns tp
      mkRuleTerm (TC.JavaValue _ _) = error "internal: Java value given to mkRuleTerm"
      mkRuleTerm (TC.Var name tp) = mkVar name tp
  let rl = Rule ruleName (evalTerm (mkRuleTerm lhsExpr)) (evalTerm (mkRuleTerm rhsExpr))
  modify $ \s -> s { rules = Map.insert ruleName rl (rules s)
                   , enabledRules = Set.insert ruleName (enabledRules s) }
  debugWrite $ "Finished defining rule " ++ ruleName
execute (AST.Disable pos name) = do
  checkNameIsDefined pos name
  modify $ \s -> s { enabledRules = Set.delete name (enabledRules s) }
execute (AST.Enable pos name) = do
  checkNameIsDefined pos name
  modify $ \s -> s { enabledRules = Set.insert name (enabledRules s) }

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
        , definedNames = Map.empty
        , sawOpMap     = Map.fromList [ ("aget", getArrayValueOpDef)
                                      , ("aset", setArrayValueOpDef)]
        , sbvOpMap     = Map.empty
        , methodSpecs  = []
        , rules        = Map.empty
        , enabledRules = Set.empty
        , globalLetBindings = Map.empty
        }
      Ex action = do cmds <- parseFile initialPath
                     mapM_ execute cmds
                     liftIO $ putStrLn "Verification complete!"
                     return ExitSuccess
  catch (evalStateT action initState)
    (\(ExecException absPos errorMsg resolution) -> do
        relPos <- posRelativeToCurrentDirectory absPos
        ts <- getTimeStamp
        putStrLn $ "\n[" ++ ts ++ "] Verification failed!\n"
        putStrLn $ show relPos
        let rend = renderStyle style { lineLength = 100 }
        putStrLn $ rend $ nest 2 errorMsg
        when (resolution /= "") $ do
          putStrLn ""
          putStrLn $ rend $ nest 2 $ ftext resolution
        return $ ExitFailure (-1))
