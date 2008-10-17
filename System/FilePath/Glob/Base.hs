-- File created: 2008-10-10 13:29:26

module System.FilePath.Glob.Base where

import Data.Maybe      (fromMaybe)
import System.FilePath (pathSeparator, extSeparator)

-- Todo? data Options = Options { allow_dots :: Bool }

data Token
   -- primitives
   = Literal !Char
   | ExtSeparator                            -- .
   | PathSeparator                           -- /
   | NonPathSeparator                        -- ?
   | CharRange [Either Char (Char,Char)]     -- []
   | OpenRange (Maybe String) (Maybe String) -- <>
   | AnyNonPathSeparator                     -- *
   | AnyDirectory                            -- **/

   -- after optimization only
   | LongLiteral !Int String

newtype Pattern = Pattern { unPattern :: [Token] }

liftP :: ([Token] -> [Token]) -> Pattern -> Pattern
liftP f (Pattern pat) = Pattern (f pat)

instance Show Token where
   show (Literal c)         = [c]
   show ExtSeparator        = [ extSeparator]
   show PathSeparator       = [pathSeparator]
   show NonPathSeparator    = "?"
   show AnyNonPathSeparator = "*"
   show AnyDirectory        = "**/"
   show (LongLiteral _ s)   = s
   show (CharRange r)       =
      '[' : concatMap (either (:[]) (\(a,b) -> [a,'-',b])) r ++ "]"
   show (OpenRange a b)     =
      '<' : fromMaybe "" a ++ "-" ++
            fromMaybe "" b ++ ">"

   showList = showList . concatMap show

instance Show Pattern where
   showsPrec d (Pattern ts) =
      showParen (d > 10) $ showString "compile " . shows ts
