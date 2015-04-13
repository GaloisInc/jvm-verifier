{- |
Module           : $Header$
Description      :
Stability        : stable
Point-of-contact : jhendrix, jstanley
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
module Verifier.Java.Codebase
  ( Codebase
  , getClasses
  , isStrictSuper
  , isSubtype
  , loadCodebase
  , locateField
  , tryLookupClass
  , lookupClass
  , findStaticMethodsByRef
  , findVirtualMethodsByRef
  , lookupSymbolicMethod
  , lookupSymbolicMethod'
  , supers
  , module Language.JVM.Parser
  ) where

import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.Map as M
import Data.IORef
import Data.Maybe
import System.Directory
import System.FilePath

import Text.PrettyPrint

import Data.JVM.Symbolic.Translation
import Language.JVM.Common
import Language.JVM.JarReader
import Language.JVM.Parser
import Verifier.Java.Utils

-- | Collection of classes loaded by JVM.
data CodebaseState = CodebaseState {
    jarReader   :: JarReader
  -- ^ Maps class names to a lazily loaded class
  , classMap    :: M.Map String Class
  , subclassMap :: M.Map String [Class]
  -- ^ Maps class names to the list of classes that are direct subclasses, and
  -- interfaces to list of classes that directly implement them.
  , symbolicMethodMap :: M.Map (String, MethodKey) [SymBlock]
  }

newtype Codebase = Codebase (IORef CodebaseState)

instance Show Codebase where
  show _ = "Codebase XXXXXX"

-- | Loads Java classes directly beneath given path.  Also loads jar indices for
-- lazy class loading.
loadCodebase :: [FilePath] -> [FilePath] -> IO Codebase
loadCodebase jarFiles classPaths = do
  -- REVISIT: Currently, classes found in the classpath shadow those in the
  -- jars.  Pretty sure the -classpath argument to the vanilla jvm allows
  -- mixture of jars and directories, and resolves names in the order in which
  -- those elements are encountered.  We probably want to do the same thing (and
  -- be able to just provide one argument to loadCodebase), but not for the
  -- beta. [js 04 Nov 2010]
  jars       <- newJarReader jarFiles
  classFiles <- filter (\p -> takeExtension p == ".class")
                  <$> recurseDirectories classPaths
  classes <- mapM loadClass classFiles
  let cb = foldr addClass (CodebaseState jars M.empty M.empty M.empty) classes
  fmap Codebase $ newIORef cb

-- Returns in-order listing of all directories and files beneath the given list of paths.
-- WARNING: May return an infinite list when symlinks are encountered.
recurseDirectories :: [FilePath] -> IO [FilePath]
recurseDirectories paths = impl paths []
  where impl [] result = return (reverse result)
        impl (path : rest) result = do
          subpaths <- getSubdirectories path
          seq (length subpaths) $ impl (subpaths ++ rest) (path : result)
        -- Returns directories and files directly beneath path if any.
        getSubdirectories :: FilePath -> IO [FilePath]
        getSubdirectories path = do
          exists <- doesDirectoryExist path
          if exists
          then do
            p <- getPermissions path
            if readable p
            then do contents <- getDirectoryContents path
                    return
                      $ map (path </>)
                      $ filter (\path' -> path' `seq` (path' /= "." && path' /= ".."))
                      $ contents
            else return []
          else return []

-- | Register a class with the given codebase
addClass :: Class -> CodebaseState -> CodebaseState
addClass cl (CodebaseState jr cMap scMap symMap) =
  CodebaseState jr
                (M.insert (className cl) cl cMap)
                (foldr addToSuperclass scMap
                   (maybeToList (superClass cl)++classInterfaces cl))
                symMap
  where addToSuperclass super m =
          M.alter (\subclasses -> case subclasses of
                                    Just list -> Just (cl : list)
                                    Nothing -> Just [cl])
                  super
                  m

-- | Returns class with given name in codebase or returns nothing if no class with
-- that name can be found.
tryLookupClass :: Codebase -> String -> IO (Maybe Class)
tryLookupClass (Codebase cbRef) clNm = do
  cb <- readIORef cbRef
  case M.lookup clNm (classMap cb) of
    Just cl -> return (Just cl)
    Nothing -> do
      mcl <- loadClassFromJar clNm (jarReader cb)
      case mcl of
        Just cl -> do
          writeIORef cbRef $! addClass cl cb
          return $ Just cl
        Nothing -> return Nothing

-- | Returns class with given name in codebase or raises error if no class with
-- that name can be found.
lookupClass :: Codebase -> String -> IO Class
lookupClass cb clNm = do
  maybeCl <- tryLookupClass cb clNm
  case maybeCl of
    Just cl -> return cl
    Nothing -> error $ "Cannot find class " ++ slashesToDots clNm ++ " in codebase."

getClasses :: Codebase -> IO [Class]
getClasses (Codebase cbRef) = do
  cb <- readIORef cbRef
  return . M.elems . classMap $ cb

-- | Adjusts the given field id to specify as its class the class in the
-- superclass hierarchy that declares it
locateField :: Codebase -> FieldId -> IO FieldId
locateField cb fldId = do
  owner <- findFieldOwner fldId
  return $ fldId { fieldIdClass = className owner}
  where
    -- Walk an inheritance hierarchy to determine the the class that declares a
    -- given field (i.e., the "field owner")
    findFieldOwner :: FieldId -> IO Class
    findFieldOwner FieldId{fieldIdName = fldNm, fieldIdClass = clNm } = do
      sups <- supers cb =<< lookupClass cb clNm
      case filter hasField sups of
        -- In theory, this should be unreachable.
        [] -> error $ "Unable to find field '" ++ fldNm
                    ++ "' in superclass hierarchy of class " ++ clNm
        (cl' : _) -> return cl'
      where
        hasField cl = fldNm `elem` map fieldName accessibleFields
          where accessibleFields
                  | className cl == clNm = classFields cl
                  | otherwise =
                      filter ((/=) Private . fieldVisibility) (classFields cl)

-- | (isStrictSuper cb name class) returns true if name is a (strict) superclass
-- of class in cb.
isStrictSuper :: Codebase -> String -> Class -> IO Bool
isStrictSuper cb name cl = do
  case superClass cl of
    Just super 
      | name == super -> return True
      | otherwise -> isStrictSuper cb name =<< lookupClass cb super
    Nothing -> return False

-- | Returns true if subclass is a subtype of superclass in codebase.
isSubtype :: Codebase -> Type -> Type -> IO Bool
isSubtype _ sub super | sub == super           = return True
isSubtype cb (ArrayType sub) (ArrayType super)  = isSubtype cb sub super
isSubtype _ (ArrayType _sub) (ClassType super) =
  return $ super == "java/lang/Object"
           || super == "java/lang/Cloneable"
           || super == "java/io/Serializable"
isSubtype cb (ClassType subName) super@(ClassType _) = do
 subclass <- lookupClass cb subName
 -- Check if sub is a subclass of super
 b <- case (superClass subclass) of
        Just superName -> isSubtype cb (ClassType superName) super
        Nothing        -> return False
 -- Check if super is an interface that sub implements
 b' <- or <$> mapM (\i -> isSubtype cb (ClassType i) super)
                   (classInterfaces subclass)
 return $ b || b'
isSubtype _ _sub _super = return False

-- | Finds all classes that implement a given method. List is ordered so that
-- subclasses appear before their base class.
findVirtualMethodsByRef :: Codebase
                        -> String -- ^ Name of class given in code for this class.
                        -> MethodKey -- ^ Method to identify.
                        -> String -- ^ Concrete class for this object.
                        -> IO [String]
findVirtualMethodsByRef cb name key instTyNm = do
  cl <- lookupClass cb name
  sups    <- drop 1 <$> supers cb cl
  revSubs <- reverse <$> cb `subs` cl
  map className <$> filterM isMatch (revSubs ++ sups)
 where
    isMatch cl = case cl `lookupMethod` key of
      Nothing -> return False
      Just _  -> do
        b   <- isStrictSuper cb (className cl) =<< lookupClass cb instTyNm
        return $ className cl == instTyNm || b

-- | Finds all classes that implement a given static method. List is
-- ordered so that subclasses appear before their base class.
findStaticMethodsByRef :: Codebase
                       -> String 
                       -- ^ Name of class given in the code for this call
                       -> MethodKey
                       -- ^ Method to call
                       -> IO [String]                          
findStaticMethodsByRef cb name key = do
  cl <- lookupClass cb name
  sups <- supers cb cl
  let isMatch clName = isJust (clName `lookupMethod` key)
  return . map className . filter isMatch $ sups

-- | Produces the superclass hierarchy of the given class. Ordered from subclass
-- to base class, starting with the given class.
supers :: Codebase -> Class -> IO [Class]
supers cb cl = do
  starClosureM (maybe (return []) (fmap (:[]) . lookupClass cb) . superClass) cl

-- | Produces the subclass hierarchy of the given class.  Ordered
-- from base class to subclass, starting with the given class.
subs :: Codebase -> Class -> IO [Class]
subs (Codebase ref) cl = do
  cb <- readIORef ref
  return $ starClosure (maybe [] id . (`M.lookup` subclassMap cb) . className) cl

lookupSymbolicMethod' :: Class -> MethodKey -> Maybe ([SymBlock], [SymTransWarning])
lookupSymbolicMethod' cl key = liftCFG <$> (methodCFG =<< lookupMethod cl key)
  where methodCFG method = case methodBody method of
          Code _ _ cfg _ _ _ _ -> Just cfg
          _                    -> Nothing

lookupSymbolicMethod :: Codebase -> String -> MethodKey -> IO (Maybe [SymBlock])
lookupSymbolicMethod (Codebase ref) clName key = do
  cb <- readIORef ref
  case M.lookup (clName, key) (symbolicMethodMap cb) of
    Just symblocks -> return $ Just symblocks
    Nothing -> do 
      cl <- lookupClass (Codebase ref) clName
      case lookupSymbolicMethod' cl key of
        Nothing -> do dbugM . render $ "unable to translate" <+> ppMethodKey key
                      return Nothing
        Just (symblocks, warns) -> do
            let map' = M.insert (clName, key) symblocks (symbolicMethodMap cb)
            writeIORef ref $ cb { symbolicMethodMap = map' }
            case warns of
              [] -> return ()
              _  -> dbugM . render $ "warnings translating" <+> ppMethodKey key
                                     $+$ nest 2 (sep warns)
            return $ Just symblocks

--------------------------------------------------------------------------------
-- Misc

-- (starClosure f a) returns the list of values obtainable by 0 or more
-- applications of f.
starClosure :: (a -> [a]) -> a -> [a]
starClosure fn a = a : concatMap (starClosure fn) (fn a)

-- The monadic variant of starClosure
starClosureM :: Monad m => (a -> m [a]) -> a -> m [a]
starClosureM fn a =
  return ((a:) . concat) `ap` (mapM (starClosureM fn) =<< fn a)
