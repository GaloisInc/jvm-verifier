{- |
Module           : $Header$
Description      :
Stability        : stable
Point-of-contact : jhendrix, jstanley
-}

{-# LANGUAGE FlexibleContexts #-}

module Execution.Codebase
  (
    Codebase
  , HasCodebase(..)
  , isStrictSuper
  , isSubtype
  , loadCodebase
  , locateField
  , tryLookupClass
  , lookupClass
  , findVirtualMethodsByRef
  , supers
  )

where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.IORef
import Data.Maybe
import System.Directory
import System.FilePath

import qualified Data.Map as M

import JarReader
import JavaParser
import Utils.Common

class (Functor m, MonadIO m) => HasCodebase m where
  getCodebase :: m Codebase

-- | Collection of classes loaded by JVM.
data CodebaseState = CodebaseState {
    jarReader   :: JarReader
  -- ^ Maps class names to a lazily loaded class
  , classMap    :: M.Map String Class
  , subclassMap :: M.Map String [Class]
  -- ^ Maps class names to the list of classes that are direct subclasses, and
  -- interfaces to list of classes that directly implement them.
  } deriving (Show)

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
  let cb = foldr addClass (CodebaseState jars M.empty M.empty) classes
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
            then do contents <- getDirectoryContents path
                    return
                      $ map ((path ++ "/") ++)
                      $ filter (\path' -> path' `seq` (path' /= "." && path' /= ".."))
                      $ contents
            else return []

-- | Register a class with the given codebase
addClass :: Class -> CodebaseState -> CodebaseState
addClass cl (CodebaseState jr cMap scMap) =
  CodebaseState jr
                (M.insert (className cl) cl cMap)
                (foldr addToSuperclass scMap
                   (maybeToList (superClass cl)++classInterfaces cl))
  where addToSuperclass super m =
          M.alter (\subclasses -> case subclasses of
                                    Just list -> Just (cl : list)
                                    Nothing -> Just [cl])
                  super
                  m

getCodebaseState :: HasCodebase m => m CodebaseState
getCodebaseState = do
  Codebase cbRef <- getCodebase
  liftIO $ readIORef cbRef

putCodebaseState :: HasCodebase m => CodebaseState -> m ()
putCodebaseState cb = do
  Codebase cbRef <- getCodebase
  cb `seq` liftIO (writeIORef cbRef cb)

-- | Returns class with given name in codebase or returns nothing if no class with
-- that name can be found.
tryLookupClass :: HasCodebase m => String -> m (Maybe Class)
tryLookupClass clNm = do
  cb <- getCodebaseState
  case M.lookup clNm (classMap cb) of
    Just cl -> return (Just cl)
    Nothing -> do
      mcl <- liftIO $ loadClassFromJar clNm (jarReader cb)
      case mcl of
        Just cl -> do
          putCodebaseState $ addClass cl cb
          return $ Just cl
        Nothing -> return Nothing

-- | Returns class with given name in codebase or raises error if no class with
-- that name can be found.
lookupClass :: (MonadIO m, HasCodebase m) => String -> m Class
lookupClass clNm = do
  maybeCl <- tryLookupClass clNm
  case maybeCl of
    Just cl -> return cl
    Nothing ->
      error $ "Cannot find class " ++ slashesToDots clNm ++ " in codebase."

-- | Adjusts the given field id to specify as its class the class in the
-- superclass hierarchy that declares it
locateField :: (Functor m, MonadIO m, HasCodebase m) =>
               FieldId -> m FieldId
locateField fldId = do
  owner <- findFieldOwner fldId
  return $ fldId{ fieldIdClass = className owner}
  where
    -- Walk an inheritance hierarchy to determine the the class that declares a
    -- given field (i.e., the "field owner")
    findFieldOwner :: (Functor m, MonadIO m, HasCodebase m) =>
                      FieldId -> m Class
    findFieldOwner FieldId{fieldIdName = fldNm, fieldIdClass = clNm } = do
      sups <- supers =<< lookupClass clNm
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
isStrictSuper :: (MonadIO m, HasCodebase m) => String -> Class -> m Bool
isStrictSuper name cl =
  case superClass cl of
    Just super -> do
      iss <- isStrictSuper name =<< lookupClass super
      return $ name == super || iss
    Nothing -> return False

-- | Returns true if subclass is a subtype of superclass in codebase.
isSubtype :: (Functor m, MonadIO m, HasCodebase m) =>
             Type -> Type -> m Bool
isSubtype sub super | sub == super           = return True
isSubtype (ArrayType sub) (ArrayType super)  = isSubtype sub super
isSubtype (ArrayType _sub) (ClassType super) =
  return $ super == "java/lang/Object"
           || super == "java/lang/Cloneable"
           || super == "java/io/Serializable"
isSubtype (ClassType subName) super@(ClassType _) = do
 subclass <- lookupClass subName
 -- Check if sub is a subclass of super
 b <- case (superClass subclass) of
        Just superName -> isSubtype (ClassType superName) super
        Nothing        -> return False
 -- Check if super is an interface that sub implements
 b' <- or <$> mapM (\i -> isSubtype (ClassType i) super)
                   (classInterfaces subclass)
 return $ b || b'
isSubtype _sub _super = return False

-- | Finds all classes that implement a given method. List is ordered so that
-- subclasses appear before their base class.
findVirtualMethodsByRef :: (Functor m, MonadIO m, HasCodebase m)
                        => String -- ^ Name of class given in code for this class.
                        -> MethodKey -- ^ Method to identify.
                        -> String -- ^ Concrete class for this object.
                        -> m [String]
findVirtualMethodsByRef name key instTyNm = do
  cl      <- lookupClass name
  sups    <- drop 1 <$> supers cl
  cb <- getCodebase
  revSubs <- liftIO $ fmap reverse $ cb `subs` cl
  map className <$> filterM isMatch (revSubs ++ sups)
  where
    isMatch cl = case cl `lookupMethod` key of
      Nothing -> return False
      Just _  -> do
        b   <- isStrictSuper (className cl) =<< lookupClass instTyNm
        return $ className cl == instTyNm || b

-- | Produces the superclass hierarchy of the given class. Ordered from subclass
-- to base class, starting with the given class.
supers :: HasCodebase m => Class -> m [Class]
supers = starClosureM $
  maybe (return []) (fmap (:[]) . lookupClass) . superClass

-- | Produces the subclass hierarchy of the given class.  Ordered
-- from base class to subclass, starting with the given class.
subs :: Codebase -> Class -> IO [Class]
subs (Codebase ref) cl = do
  cb <- readIORef ref
  return $ starClosure (maybe [] id . (`M.lookup` subclassMap cb) . className) cl

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
