-- File created: 2008-10-10 13:29:26

module System.FilePath.Glob.Base where

-- Todo: data Options = Options { allow_dots :: Bool }

data Token
   -- primitives
   = Literal Char
   | ExtSeparator                              -- .
   | PathSeparator                             -- /
   | NonPathSeparator                          -- ?
   | CharRange [Either Char (Char,Char)]       -- []
   | OpenRange (Maybe Integer) (Maybe Integer) -- <>
   | AnyNonPathSeparator                       -- *
   | AnyDirectory                              -- **/

   -- after optimization only
   | LongLiteral Int String                    -- also "pre-optimized": <a-a>
--   | AnyDirectoryAtDepth Int             -- */*/*/.../
   deriving (Show)

newtype Pattern = Pattern { unPattern :: [Token] } deriving (Show)

liftP :: ([Token] -> [Token]) -> Pattern -> Pattern
liftP f (Pattern pat) = Pattern (f pat)
