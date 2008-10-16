-- File created: 2008-10-16 12:12:50

module System.FilePath.Glob.Directory (globDir) where

import Control.Monad     (forM)
import Data.List         ((\\), partition, tails)
import System.Directory  (doesDirectoryExist, getDirectoryContents)
import System.FilePath   ((</>), splitPath)

import System.FilePath.Glob.Base
import System.FilePath.Glob.Match (match)
import System.FilePath.Glob.Utils (getRecursiveContents, pathParts)

-- The Patterns in TypedPattern don't contain PathSeparator or AnyDirectory
data TypedPattern
   = Any Pattern    -- pattern
   | Dir Pattern    -- pattern/
   | AnyDir Pattern -- pattern**/
   deriving Show

-- |Matches each given @Pattern@ against the contents of the given @FilePath@,
-- recursively. The result pair's first component contains the matched paths,
-- grouped for each given @Pattern@, and the second contains all paths which
-- were not matched by any @Pattern@.
--
-- If multiple @Pattern@s match a single @FilePath@, that path will be included
-- in multiple groups.
--
-- This function is different from a simple @filter@ over all the contents of
-- the directory: the matching is performed relative to the directory, so that
-- for instance the following is true:
--
--     fmap (head.fst) (globDir [compile "*"] dir) == getDirectoryContents dir
--
-- If 'dir' is '\"foo\"' the pattern should be '\"foo/*\"' to get the same
-- results with a plain @filter@.
--
-- The results are enumerated lazily, using @unsafeInterleaveIO@.
globDir :: [Pattern] -> FilePath -> IO ([[FilePath]], [FilePath])
globDir pats dir = do
   results <- mapM (\p -> globDir' (separate p) dir) pats

   let (matches, others) = unzip results

   return (matches, concat others)

globDir' :: [TypedPattern] -> FilePath -> IO ([FilePath], [FilePath])
globDir' pats dir = do
   raw <- getDirectoryContents dir

   let entries = raw \\ [".",".."]

   results <- forM entries $ \e -> matchTypedAndGo pats e (dir </> e)

   let (matches, others) = unzip results

   return (concat matches, concat others)

matchTypedAndGo :: [TypedPattern]
                -> FilePath -> FilePath
                -> IO ([FilePath], [FilePath])

matchTypedAndGo [] _ _ = return ([], [])

-- (Any p) is always the last element
matchTypedAndGo [Any p] path absPath =
   if match p path
      then return ([absPath], [])
      else doesDirectoryExist absPath >>= didn'tMatch absPath

matchTypedAndGo (Dir p:ps) path absPath = do
   isDir <- doesDirectoryExist absPath
   if isDir && match p path
      then globDir' ps absPath
      else didn'tMatch absPath isDir

matchTypedAndGo (AnyDir p:ps) path absPath = do
   isDir <- doesDirectoryExist absPath
   if isDir && (null (unPattern p) || match p path)
      then do
         entries <- getRecursiveContents absPath
         let pat = unseparate ps
         return (partition (any (match pat) . pathParts) entries)
      else
         didn'tMatch absPath isDir

didn'tMatch :: FilePath -> Bool -> IO ([FilePath], [FilePath])
didn'tMatch absPath isDir = (fmap $ (,) []) $
   if isDir
      then getRecursiveContents absPath
      else return [absPath]

separate :: Pattern -> [TypedPattern]
separate = go [] . unPattern
 where
   go [] []                 = []
   go gr []                 = [Any    $ f gr]
   go gr (PathSeparator:ps) = (Dir    $ f gr) : go [] ps
   go gr ( AnyDirectory:ps) = (AnyDir $ f gr) : go [] ps
   go gr (            p:ps) = go (p:gr) ps

   f = Pattern . reverse

unseparate :: [TypedPattern] -> Pattern
unseparate = Pattern . foldr f []
 where
   f (AnyDir p) ts = unPattern p ++ AnyDirectory  : ts
   f (   Dir p) ts = unPattern p ++ PathSeparator : ts
   f (Any    p) ts = unPattern p ++ ts
