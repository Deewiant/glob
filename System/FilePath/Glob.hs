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
   , tryCompile, compile
     -- ** Matching
   , match
   , globDir
   ) where

import System.FilePath.Glob.Base      (Pattern)
import System.FilePath.Glob.Compile   (compile, tryCompile)
import System.FilePath.Glob.Directory (globDir, factorPath)
import System.FilePath.Glob.Match     (match)
