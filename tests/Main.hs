-- File created: 2008-10-10 16:23:56

module Main (main) where

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.QuickCheck

import Utils

testOptions = mempty `mappend`
   TestOptions Nothing (Just 1000) Nothing Nothing

runnerOptions = mempty `mappend`
   RunnerOptions Nothing (Just testOptions) Nothing

main = defaultMainWithOpts tests runnerOptions

tests =
   [ testGroup "Utils"
      [ testProperty "overlapperLosesNoInfo" prop_overlapperLosesNoInfo
      , testProperty "increasingSeq-1"       prop_increasingSeq1
      ]
   ]
