-- File created: 2008-10-11 11:18:31

module Tests.Optimizer (tests) where

import Test.Framework
import Test.Framework.Providers.QuickCheck

import System.FilePath.Glob.Compile
import System.FilePath.Glob.Optimize
import System.FilePath.Glob.Match

import Tests.Base

tests =
   [ testGroup "Optimizer"
       [ testProperty "optimize-1" prop_optimize1
       , testProperty "optimize-2" prop_optimize2
       ]
   ]

-- Optimizing twice should give the same result as optimizing once
prop_optimize1 s =
   let pat = tokenize (unPS s)
       xs = iterate optimize (fromRight pat)
    in isRight pat && show (xs !! 1) == show (xs !! 2)

-- Optimizing shouldn't affect whether a match succeeds
prop_optimize2 p s =
   let x   = tokenize (unPS p)
       pat = fromRight x
       pth = unP s
    in isRight x && match pat pth == match (optimize pat) pth
