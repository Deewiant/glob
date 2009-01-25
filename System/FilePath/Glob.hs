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
   , factorPath
     -- ** Compilation
   , compile, simplify
     -- *** Options
   , CompOptions(..)
   , tryCompileWith, compileWith
     -- **** Predefined option sets
   , compExtended, compPosix
     -- ** Matching
   , match
   , globDir
   ) where

import System.FilePath.Glob.Base      ( Pattern, CompOptions(..)
                                      , compExtended, compPosix
                                      )
import System.FilePath.Glob.Compile   (compile, compileWith, tryCompileWith)
import System.FilePath.Glob.Directory (globDir, factorPath)
import System.FilePath.Glob.Match     (match)
import System.FilePath.Glob.Optimize  (simplify)
