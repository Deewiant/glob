-- File created: 2008-10-10 13:29:26

module System.FilePath.Glob.Base where

import Control.Exception (assert)
import Data.Maybe        (fromMaybe)
import System.FilePath   ( pathSeparator, extSeparator
                         , isExtSeparator, isPathSeparator
                         )

-- Todo? data Options = Options { allow_dots :: Bool }

data Token
   -- primitives
   = Literal !Char
   | ExtSeparator                              --  .
   | PathSeparator                             --  /
   | NonPathSeparator                          --  ?
   | CharRange !Bool [Either Char (Char,Char)] --  []
   | OpenRange (Maybe String) (Maybe String)   --  <>
   | AnyNonPathSeparator                       --  *
   | AnyDirectory                              --  **/

   -- after optimization only
   | LongLiteral !Int String
   deriving (Eq)

-- |An abstract data type representing a compiled pattern.
-- 
-- The 'Show' instance is essentially the inverse of @'compile'@. Though it may
-- not return exactly what was given to @'compile'@ it will return code which
-- produces the same 'Pattern'.
--
-- Note that the 'Eq' instance cannot tell you whether two patterns behave in
-- the same way; only whether they compile to the same 'Pattern'. For instance,
-- @'compile' \"x\"@ and @'compile' \"[x]\"@ may or may not compare equal,
-- though a @'match'@ will behave the exact same way no matter which 'Pattern'
-- is used.
newtype Pattern = Pattern { unPattern :: [Token] } deriving (Eq)

liftP :: ([Token] -> [Token]) -> Pattern -> Pattern
liftP f (Pattern pat) = Pattern (f pat)

instance Show Token where
   show (Literal c)
      | c `elem` "*?[<" || isExtSeparator c
                            = ['[',c,']']
      | otherwise           = assert (not $ isPathSeparator c) [c]
   show ExtSeparator        = [ extSeparator]
   show PathSeparator       = [pathSeparator]
   show NonPathSeparator    = "?"
   show AnyNonPathSeparator = "*"
   show AnyDirectory        = "**/"
   show (LongLiteral _ s)   = s
   show (CharRange b r)     =
      '[' : (if b then "" else "^") ++
            concatMap (either (:[]) (\(x,y) -> [x,'-',y])) r ++ "]"
   show (OpenRange a b)     =
      '<' : fromMaybe "" a ++ "-" ++
            fromMaybe "" b ++ ">"

   showList = showList . concatMap show

instance Show Pattern where
   showsPrec d (Pattern ts) =
      showParen (d > 10) $ showString "compile " . shows ts
