-- File created: 2008-10-11 11:18:31

module Tests.Optimizer (tests) where

import Test.Framework
import Test.Framework.Providers.QuickCheck

import System.FilePath.Glob.Base  (tokenize, optimize)
import System.FilePath.Glob.Match

import Tests.Base

tests = testGroup "Optimizer"
   [ testProperty "optimize-1" prop_optimize1
   , testProperty "optimize-2" prop_optimize2
   ]

-- Optimizing twice should give the same result as optimizing once
prop_optimize1 o s =
   let pat = tokenize (unCOpts o) (unPS s)
       xs = iterate optimize (fromRight pat)
    in isRight pat && xs !! 1 == xs !! 2

-- Optimizing shouldn't affect whether a match succeeds
prop_optimize2 o p s =
   let x   = tokenize (unCOpts o) (unPS p)
       pat = fromRight x
       pth = unP s
    in isRight x && match pat pth == match (optimize pat) pth
