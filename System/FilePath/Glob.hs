-- File created: 2008-10-10 13:37:42

-- | A library for globbing: matching patterns against file paths.
--
-- Basic usage: @'match' ('compile' pattern) filepath@.
--
-- Basic usage in IO: @'globDir' ['compile' pattern] directory@.
module System.FilePath.Glob
   ( -- * Data type
     Pattern
     -- * Functions
   , commonDirectory
     -- ** Compilation
   , compile, simplify, decompile
     -- *** Options
   , CompOptions(..)
   , tryCompileWith, compileWith
     -- **** Predefined option sets
   , compDefault, compPosix
     -- ** Matching
   , match
   , globDir
     -- *** Options
   , MatchOptions(..)
   , matchWith
   , globDirWith
     -- **** Predefined option sets
   , matchDefault, matchPosix
   ) where

import System.FilePath.Glob.Base      ( Pattern
                                      , CompOptions(..), MatchOptions(..)
                                      , compDefault, compPosix
                                      , matchDefault, matchPosix
                                      , compile, compileWith, tryCompileWith
                                      , decompile
                                      )
import System.FilePath.Glob.Directory (globDir, globDirWith, commonDirectory)
import System.FilePath.Glob.Match     (match, matchWith)
import System.FilePath.Glob.Simplify  (simplify)
