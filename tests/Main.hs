-- File created: 2008-10-10 16:23:56

module Main (main) where

import System.Environment (getArgs)
import Test.Framework
import Test.Framework.Providers.QuickCheck

import Compiler
import Optimizer
import Utils

main = do
   args <- getArgs
   defaultMainWithArgs tests . concat $
      [ ["--timeout", show 10]
      , ["--maximum-generated-tests", show 1000]
      , args
      ]

tests =
   [ testGroup "Utils"
      [ testProperty "overlapperLosesNoInfo" prop_overlapperLosesNoInfo
      , testProperty "increasingSeq"         prop_increasingSeq
      , testProperty "addToRange"            prop_addToRange
      ]
   , testGroup "Compiler"
      [ testProperty "decompileCompile"  prop_decompileCompile
      ]
   , testGroup "Optimizer"
      [ testProperty "optimize-1" prop_optimize1
      , testProperty "optimize-2" prop_optimize2
      ]
   ]
