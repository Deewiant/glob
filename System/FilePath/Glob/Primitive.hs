-- File created: 2009-01-17

module System.FilePath.Glob.Primitive
   ( literal, wildcard, singleWildcard, recursiveWildcard ) where

import System.FilePath (isPathSeparator, isExtSeparator)

import System.FilePath.Glob.Base

literal :: String -> Pattern
literal [] = Pattern []
literal s = optimize $ Pattern $ map lit' s
          where lit' c | isPathSeparator c = PathSeparator
                       | isExtSeparator c  = ExtSeparator
                       | otherwise = Literal c

singleWildcard :: Pattern -- ^ @?@
singleWildcard = Pattern NonPathSeparator

wildcard :: Patern -- ^ @*@
wildcard = Pattern AnyNonPathSeparator

recursiveWildcard :: Pattern -- ^ @**/@
recursiveWildcard = Pattern AnyDirectory
