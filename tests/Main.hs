-- File created: 2008-10-10 16:23:56

module Main (main) where

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck

import Compiler
import Optimizer
import Utils

testOptions = mempty `mappend`
   TestOptions Nothing (Just 1000) Nothing (Just (Just (10^7)))

runnerOptions = mempty `mappend`
   RunnerOptions Nothing (Just testOptions) Nothing

main = defaultMainWithOpts tests runnerOptions

tests =
   [ testGroup "Utils"
      [ testProperty "overlapperLosesNoInfo" prop_overlapperLosesNoInfo
      , testProperty "increasingSeq"         prop_increasingSeq
      , testProperty "addToRange"            prop_addToRange
      ]
   , testGroup "Compiler"
      [ testProperty "decompileTokenize" prop_decompileTokenize
      , testProperty "decompileCompile"  prop_decompileCompile
      ]
   , testGroup "Optimizer"
      [ testProperty "optimize-1" prop_optimize1
      , testProperty "optimize-2" prop_optimize2
      ]
   ]
