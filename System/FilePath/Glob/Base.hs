-- File created: 2008-10-10 13:29:26

module System.FilePath.Glob.Base where

-- Todo? data Options = Options { allow_dots :: Bool }

data Token
   -- primitives
   = Literal Char
   | ExtSeparator                            -- .
   | PathSeparator                           -- /
   | NonPathSeparator                        -- ?
   | CharRange [Either Char (Char,Char)]     -- []
   | OpenRange (Maybe String) (Maybe String) -- <>
   | AnyNonPathSeparator                     -- *
   | AnyDirectory                            -- **/

   -- after optimization only
   | LongLiteral Int String
   deriving (Show)

newtype Pattern = Pattern { unPattern :: [Token] } deriving (Show)

liftP :: ([Token] -> [Token]) -> Pattern -> Pattern
liftP f (Pattern pat) = Pattern (f pat)
