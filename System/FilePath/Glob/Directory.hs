-- File created: 2008-10-16 12:12:50

module System.FilePath.Glob.Directory (globDir, factorPath) where

import Control.Arrow    (first)
import Control.Monad    (forM)
import qualified Data.DList as DL
import Data.DList       (DList)
import Data.List        ((\\))
import System.Directory ( doesDirectoryExist, getDirectoryContents
                        , getCurrentDirectory
                        )
import System.FilePath  ((</>), extSeparator)

import System.FilePath.Glob.Base
import System.FilePath.Glob.Match (match)
import System.FilePath.Glob.Utils
   (getRecursiveContents, nubOrd, pathParts, partitionDL)

-- The Patterns in TypedPattern don't contain PathSeparator or AnyDirectory
data TypedPattern
   = Any Pattern    -- pattern
   | Dir Pattern    -- pattern/
   | AnyDir Pattern -- pattern**/
   deriving Show

-- |Matches each given 'Pattern' against the contents of the given 'FilePath',
-- recursively. The result pair\'s first component contains the matched paths,
-- grouped for each given 'Pattern', and the second contains all paths which
-- were not matched by any 'Pattern'. The results are not in any defined order.
--
-- If multiple 'Pattern's match a single 'FilePath', that path will be included
-- in multiple groups.
--
-- This function is different from a simple 'filter' over all the contents of
-- the directory: the matching is performed relative to the directory, so that
-- for instance the following is true:
--
-- > fmap (head.fst) (globDir [compile "*"] dir) == getDirectoryContents dir
--
-- If @dir@ is @\"foo\"@ the pattern should be @\"foo/*\"@ to get the same
-- results with a plain 'filter'.
-- 
-- If the given 'FilePath' is @[]@, @getCurrentDirectory@ will be used.
--
-- Note that in some cases results outside the given directory may be returned:
-- for instance the @.*@ pattern matches the @..@ directory.
--
-- Any results deeper than in the given directory are enumerated lazily, using
-- 'unsafeInterleaveIO'.
--
-- Directories without read permissions are returned as entries but their
-- contents, of course, are not.
globDir :: [Pattern] -> FilePath -> IO ([[FilePath]], [FilePath])
globDir []   dir = do
   dir' <- if null dir then getCurrentDirectory else return dir
   c <- getRecursiveContents dir'
   return ([], DL.toList c)

globDir pats dir = do
   results <- mapM (\p -> globDir' (separate p) dir) pats

   let (matches, others) = unzip results
       allMatches        = DL.toList . DL.concat $ matches
       allOthers         = DL.toList . DL.concat $ others

   return ( map DL.toList matches
          , nubOrd allOthers \\ allMatches
          )

globDir' :: [TypedPattern] -> FilePath -> IO (DList FilePath, DList FilePath)
globDir' []   dir = didn'tMatch dir True
globDir' pats dir = do
   dir' <- if null dir then getCurrentDirectory else return dir
   entries <- getDirectoryContents dir'

   results <- forM entries $ \e -> matchTypedAndGo pats e (dir' </> e)

   let (matches, others) = unzip results

   return (DL.concat matches, DL.concat others)

matchTypedAndGo :: [TypedPattern]
                -> FilePath -> FilePath
                -> IO (DList FilePath, DList FilePath)

-- (Any p) is always the last element
matchTypedAndGo [Any p] path absPath =
   if match p path
      then return (DL.singleton absPath, DL.empty)
      else doesDirectoryExist absPath >>= didn'tMatch absPath

matchTypedAndGo (Dir p:ps) path absPath = do
   isDir <- doesDirectoryExist absPath
   if isDir && match p path
      then globDir' ps absPath
      else didn'tMatch absPath isDir

matchTypedAndGo (AnyDir p:ps) path absPath = do
   isDir <- doesDirectoryExist absPath
   let m = match (unseparate ps)

   case null (unPattern p) || match p path of
        True | isDir  -> fmap (partitionDL (any m . pathParts))
                              (getRecursiveContents absPath)
        True | m path -> return (DL.singleton absPath, DL.empty)
        _             -> didn'tMatch absPath isDir

matchTypedAndGo _ _ _ = error "Glob.matchTypedAndGo :: internal error"

didn'tMatch :: FilePath -> Bool -> IO (DList FilePath, DList FilePath)
didn'tMatch absPath isDir = (fmap $ (,) DL.empty) $
   if isDir
      then getRecursiveContents absPath
      else return$ DL.singleton absPath

separate :: Pattern -> [TypedPattern]
separate = go [] . unPattern
 where
   go [] []                              = []
   go gr []                              = [Any    $ f gr]
   -- ./foo should not be split into [. , foo], it's just foo
   go [] (ExtSeparator:PathSeparator:ps) = go [] ps
   go gr (             PathSeparator:ps) = (   Dir $ f gr) : go [] ps
   go gr (              AnyDirectory:ps) = (AnyDir $ f gr) : go [] ps
   go gr (                         p:ps) = go (p:gr) ps

   f = Pattern . reverse

unseparate :: [TypedPattern] -> Pattern
unseparate = Pattern . foldr f []
 where
   f (AnyDir p) ts = unPattern p ++ AnyDirectory  : ts
   f (   Dir p) ts = unPattern p ++ PathSeparator : ts
   f (Any    p) ts = unPattern p ++ ts

-- |Factors out the directory component of a 'Pattern'. Useful in conjunction
-- with 'globDir'.
factorPath :: Pattern -> (FilePath, Pattern)
factorPath pat =
   -- root is Dir (compile "") in TypedPattern form, special case that because
   -- "" </> "foo" is "foo" and not "/foo"
   let root = case unPattern pat of
                   (PathSeparator:_) -> "/"
                   _                 -> ""

       (baseDir, rest) = splitP $ separate pat

    in (root++baseDir, unseparate rest)
 where
   splitP pt@(Dir p:ps) =
      case fromConst DL.empty (unPattern p) of
           Just d  -> first (d </>) (splitP ps)
           Nothing -> ("", pt)

   splitP pt = ("", pt)

   fromConst d []                   = Just (DL.toList d)
   fromConst d (Literal c      :xs) = fromConst (d `DL.snoc` c) xs
   fromConst d (ExtSeparator   :xs) = fromConst (d `DL.snoc` extSeparator) xs
   fromConst d (LongLiteral _ s:xs) = fromConst (d `DL.append` DL.fromList s) xs
   fromConst _ _                    = Nothing
