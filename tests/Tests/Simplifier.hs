-- File created: 2009-01-24 13:02:48

module Tests.Simplifier (tests) where

import Test.Framework
import Test.Framework.Providers.QuickCheck

import System.FilePath.Glob.Compile  (tryCompile)
import System.FilePath.Glob.Optimize (simplify)
import System.FilePath.Glob.Match

import Tests.Base

tests = testGroup "Simplifier"
   [ testProperty "simplify-1" prop_simplify1
   , testProperty "simplify-2" prop_simplify2
   ]

-- Simplifying twice should give the same result as simplifying once
prop_simplify1 s =
   let pat = tryCompile (unPS s)
       xs = iterate simplify (fromRight pat)
    in isRight pat && xs !! 1 == xs !! 2

-- Simplifying shouldn't affect whether a match succeeds
prop_simplify2 p s =
   let x   = tryCompile (unPS p)
       pat = fromRight x
       pth = unP s
    in isRight x && match pat pth == match (simplify pat) pth
