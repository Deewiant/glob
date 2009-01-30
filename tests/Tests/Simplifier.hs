-- File created: 2009-01-24 13:02:48

module Tests.Simplifier (tests) where

import Test.Framework
import Test.Framework.Providers.QuickCheck

import System.FilePath.Glob.Base (tryCompileWith)
import System.FilePath.Glob.Match
import System.FilePath.Glob.Simplify

import Tests.Base

tests = testGroup "Simplifier"
   [ testProperty "simplify-1" prop_simplify1
   , testProperty "simplify-2" prop_simplify2
   ]

-- Simplifying twice should give the same result as simplifying once
prop_simplify1 o s =
   let pat = tryCompileWith (unCOpts o) (unPS s)
       xs = iterate simplify (fromRight pat)
    in isRight pat && xs !! 1 == xs !! 2

-- Simplifying shouldn't affect whether a match succeeds
prop_simplify2 p o s =
   let x   = tryCompileWith (unCOpts o) (unPS p)
       pat = fromRight x
       pth = unP s
    in isRight x && match pat pth == match (simplify pat) pth
