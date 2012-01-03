-- File created: 2008-10-10 13:37:42

-- | A library for globbing: matching patterns against file paths akin to the
-- POSIX @glob()@ function.
--
-- Pattern syntax is documented by 'compile'. To toggle features at compile
-- time, look into 'CompOptions'. To modify matching behaviour, look into
-- 'MatchOptions'.
--
-- Basic usage examples:
--
-- Matching a 'String' pattern against a 'FilePath':
--
-- @
-- 'match' ('compile' pattern) filepath
-- @
--
-- Matching a 'String' pattern against all paths in the current working
-- directory:
--
-- @
-- 'glob' pattern
-- @
--
-- Matching a 'String' pattern against all paths in a given directory (a
-- 'FilePath'):
--
-- @
-- 'globDir1' ('compile' pattern) directorypath
-- @
--
-- Matching a list of 'String' patterns against all paths in a given directory,
-- returning the matches for each pattern as well as the paths not matched by
-- any of the patterns:
--
-- @
-- 'globDir' (map 'compile' patterns) directorypath
-- @
module System.FilePath.Glob
   ( -- * Data type
     Pattern
     -- * Functions
     -- ** Compilation
   , compile, decompile, simplify
     -- *** Options
   , CompOptions(..)
   , compileWith, tryCompileWith
     -- **** Predefined option sets
   , compDefault, compPosix
     -- ** Matching
   , match
   , globDir, globDir1, glob
     -- *** Options
   , MatchOptions(..)
   , matchWith
   , globDirWith
     -- **** Predefined option sets
   , matchDefault, matchPosix
     -- ** Miscellaneous
   , commonDirectory
   ) where

import System.FilePath.Glob.Base      ( Pattern
                                      , CompOptions(..), MatchOptions(..)
                                      , compDefault, compPosix
                                      , matchDefault, matchPosix
                                      , compile, compileWith, tryCompileWith
                                      , decompile
                                      )
import System.FilePath.Glob.Directory ( globDir, globDirWith, globDir1, glob
                                      , commonDirectory
                                      )
import System.FilePath.Glob.Match     (match, matchWith)
import System.FilePath.Glob.Simplify  (simplify)
