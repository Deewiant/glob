-- File created: 2009-01-17

-- |A number of primitives from which complete 'Pattern's may be constructed.
--
-- Using this together with the functions provided by the 'Monoid' instance of
-- 'Pattern' allows for direct manipulation of 'Pattern's beyond what can be
-- done with just the 'compile' family of functions. And of course you don't
-- have to go via 'String's if you use these.
module System.FilePath.Glob.Primitive
   ( literal
   , singleWildcard, wildcard, recursiveWildcard
   ) where

import System.FilePath (isPathSeparator, isExtSeparator)

import System.FilePath.Glob.Base

-- |A 'Pattern' which matches the given 'String' literally.
--
-- Handles any embedded path and extension separators.
literal :: String -> Pattern
literal = optimize . Pattern . map f
 where
   f c | isPathSeparator c = PathSeparator
       | isExtSeparator c  = ExtSeparator
       | otherwise         = Literal c

-- |Matches any single character except a path separator: currently corresponds
-- to the @?@ operator.
singleWildcard :: Pattern -- ^ @?@
singleWildcard = Pattern NonPathSeparator

-- |Matches any number of characters up to a path separator: currently
-- corresponds to the @*@ operator.
wildcard :: Pattern
wildcard = Pattern AnyNonPathSeparator

-- |Matches any number of characters including path separators: currently
-- corresponds to the @**/@ operator.
recursiveWildcard :: Pattern
recursiveWildcard = Pattern AnyDirectory
