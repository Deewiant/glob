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
   , charRange, numberRange
   ) where

import System.FilePath (isPathSeparator, isExtSeparator)

import System.FilePath.Glob.Base (Pattern(..), Token(..), optimize)

-- |A 'Pattern' which matches the given 'String' literally.
--
-- Handles any embedded path and extension separators.
literal :: String -> Pattern
literal = optimize . Pattern . map f
 where
   f c | isPathSeparator c = PathSeparator
       | isExtSeparator c  = ExtSeparator
       | otherwise         = Literal c

-- |Matches any single character except a path separator: corresponds to the
-- @?@ operator.
singleWildcard :: Pattern
singleWildcard = Pattern [NonPathSeparator]

-- |Matches any number of characters up to a path separator: corresponds to the
-- @*@ operator.
wildcard :: Pattern
wildcard = Pattern [AnyNonPathSeparator]

-- |Matches any number of characters including path separators: corresponds to
-- the @**/@ operator.
recursiveWildcard :: Pattern
recursiveWildcard = Pattern [AnyDirectory]

-- |Matches a single character if it is within the (inclusive) range in any
-- 'Right' or if it is equal to the character in any 'Left'. Corresponds to the
-- @[]@, @[^]@ and @[!]@ operators.
--
-- If the given 'Bool' is 'False', the result of the match is inverted: the
-- match succeeds if the character does /not/ match according to the above
-- rules.
charRange :: Bool -> [Either Char (Char,Char)] -> Pattern
charRange b rs = optimize $ Pattern [CharRange b rs]

-- |Matches a number in the given range, which may be open, half-open, or
-- closed. Corresponds to the @\<\>@ operator.
numberRange :: Maybe Integer -> Maybe Integer -> Pattern
numberRange a b = Pattern [OpenRange (fmap show a) (fmap show b)]
