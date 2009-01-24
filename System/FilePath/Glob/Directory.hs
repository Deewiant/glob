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
import System.FilePath  ((</>), extSeparator, isExtSeparator, pathSeparator)

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
-- Two 'FilePath's which can be canonicalized to the same file (e.g. @\"foo\"@
-- and @\"./foo\"@) may appear separately if explicit matching on paths
-- beginning with @\".\"@ is done. Looking for @\".*/*\"@, for instance, will
-- cause @\"./foo\"@ to return as a match but @\"foo\"@ to not be matched.
--
-- This function is different from a simple 'filter' over all the contents of
-- the directory: the matching is performed relative to the directory, so that
-- for instance the following is true:
--
-- > fmap (head.fst) (globDir [compile "*"] dir) == getDirectoryContents dir
--
-- (With the exception that that glob won't match anything beginning with @.@.)
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
globDir' pats@(_:_) dir = do
   dir' <- if null dir then getCurrentDirectory else return dir
   entries <- getDirectoryContents dir'

   results <- forM entries $ \e -> matchTypedAndGo pats e (dir' </> e)

   let (matches, others) = unzip results

   return (DL.concat matches, DL.concat others)

globDir' [] dir =
   -- We can only get here from matchTypedAndGo getting a [Dir _]: it means the
   -- original pattern had a trailing PathSeparator. Reproduce it here.
   return (DL.singleton (dir ++ [pathSeparator]), DL.empty)

matchTypedAndGo :: [TypedPattern]
                -> FilePath -> FilePath
                -> IO (DList FilePath, DList FilePath)

-- (Any p) is always the last element
matchTypedAndGo [Any p] path absPath =
   if match p path
      then return (DL.singleton absPath, DL.empty)
      else doesDirectoryExist absPath >>= didn'tMatch path absPath

matchTypedAndGo (Dir p:ps) path absPath = do
   isDir <- doesDirectoryExist absPath
   if isDir && match p path
      then globDir' ps absPath
      else didn'tMatch path absPath isDir

matchTypedAndGo (AnyDir p:ps) path absPath = do
   if path `elem` [".",".."]
      then didn'tMatch path absPath True
      else do
         isDir <- doesDirectoryExist absPath
         let m = match (unseparate ps)
             unconditionalMatch =
                null (unPattern p) && not (isExtSeparator $ head path)
             p' = Pattern (unPattern p ++ [AnyNonPathSeparator])

         case unconditionalMatch || match p' path of
              True | isDir  -> do
                 contents <- getRecursiveContents absPath
                 return $
                    -- foo**/ should match foo/ and nothing below it
                    -- relies on head contents == absPath
                    if null ps
                       then (DL.singleton $ DL.head contents, DL.tail contents)
                       else partitionDL (any m . pathParts) contents

              True | m path -> return (DL.singleton absPath, DL.empty)
              _             -> didn'tMatch path absPath isDir

matchTypedAndGo _ _ _ = error "Glob.matchTypedAndGo :: internal error"

-- To be called when a pattern didn't match a path: given the path and whether
-- it was a directory, return all paths which didn't match (i.e. for a file,
-- just the file, and for a directory, everything inside it).
didn'tMatch :: FilePath -> FilePath -> Bool
            -> IO (DList FilePath, DList FilePath)
didn'tMatch path absPath isDir = (fmap $ (,) DL.empty) $
   if isDir
      then if path `elem` [".",".."]
              then return DL.empty
              else getRecursiveContents absPath
      else return$ DL.singleton absPath

separate :: Pattern -> [TypedPattern]
separate = go [] . unPattern
 where
   go [] []                              = []
   go gr []                              = [Any    $ f gr]
   -- ./foo should not be split into [. , foo], it's just foo
   go [] (ExtSeparator:PathSeparator:ps) = go [] ps
   go gr (             PathSeparator:ps) = (   Dir $ f gr) : go [] (dropSls ps)
   go gr (              AnyDirectory:ps) = (AnyDir $ f gr) : go [] (dropSls ps)
   go gr (                         p:ps) = go (p:gr) ps

   f = Pattern . reverse

   dropSls = dropWhile isSlash
    where
      isSlash PathSeparator = True
      isSlash _             = False

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
   fromConst d (LongLiteral _ s:xs) = fromConst (d `DL.append`DL.fromList s) xs
   fromConst _ _                    = Nothing
