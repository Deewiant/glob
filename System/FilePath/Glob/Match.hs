-- File created: 2008-10-10 13:29:03

module System.FilePath.Glob.Match (match) where

import Numeric         (readDec)
import System.FilePath (isPathSeparator, isExtSeparator)

import System.FilePath.Glob.Base
import System.FilePath.Glob.Utils (inRange, pathParts)

match :: Pattern -> FilePath -> Bool
match _         "." = False
match _        ".." = False
match (Pattern p) s = begMatch p s

-- begMatch takes care of some things at the beginning of a pattern or after /:
--    - . needs to be matched explicitly
--    - ./foo is equivalent to foo
begMatch, match' :: [Token] -> FilePath -> Bool
begMatch (ExtSeparator:PathSeparator:pat) s                  = begMatch pat s
begMatch pat (x:y:s) | isExtSeparator x && isPathSeparator y = begMatch pat s
begMatch pat s =
   if not (null s) && isExtSeparator (head s)
      then case pat of
                ExtSeparator:pat' -> match' pat' (tail s)
                _                 -> False
      else match' pat s

match' []                        s  = null s
match' (AnyNonPathSeparator:_)   "" = True
match' _                         "" = False
match' (Literal l       :xs) (c:cs) =                 l == c  && match'   xs cs
match' ( ExtSeparator   :xs) (c:cs) =       isExtSeparator c  && match'   xs cs
match' (PathSeparator   :xs) (c:cs) =      isPathSeparator c  && begMatch xs cs
match' (NonPathSeparator:xs) (c:cs) = not (isPathSeparator c) && match'   xs cs

match' (CharRange range :xs) (c:cs) =
   any (either (== c) (`inRange` c)) range && match' xs cs

match' (OpenRange lo hi :xs) path =
   case readDec path of
        [(n,cs)] ->
           maybe True (n >=) lo &&
           maybe True (n <=) hi && match' xs cs
        _ -> False

match' again@(AnyNonPathSeparator:xs) path@(c:cs) =
   if isPathSeparator c
      then match' xs path

      -- AnyNonPathSeparator [] is not a separate case since we don't want
      -- anything except "" to match ""; instead, check for a null tail here
      else null cs || match' again cs

match' again@(AnyDirectory:xs) path =
   let parts   = pathParts path
       matches = any (match' xs) parts || any (match' again) (tail parts)
    in if null xs
          -- **/ shouldn't match foo/.bar, so check that remaining bits don't
          -- start with .
          then all (not.isExtSeparator.head) (init parts) && matches
          else matches

match' (LongLiteral len s:xs) path =
   let (pre,cs) = splitAt len path
    in pre == s && match' xs cs
