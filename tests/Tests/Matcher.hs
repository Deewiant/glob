-- File created: 2008-10-16 16:16:06

module Tests.Matcher (tests) where

import Control.Monad (ap)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck ((==>))

import System.FilePath (isExtSeparator, isPathSeparator)
import System.FilePath.Glob.Base
import System.FilePath.Glob.Match

import Tests.Base

tests = testGroup "Matcher"
   [ testProperty "match-1" prop_match1
   , testProperty "match-2" prop_match2
   , testProperty "match-3" prop_match3
   , testProperty "match-4" prop_match4
   ]

-- ./foo should be equivalent to foo in both path and pattern
-- ... but not when exactly one of the two starts with /
prop_match1 o p_ pth_ =
   let p0    = unPS p_
       pth0  = unP pth_
       (p, pth) =
          if (not (null p0) && isPathSeparator (head p0)) /=
                (not (null pth0) && isPathSeparator (head pth0))
             then (dropWhile isPathSeparator p0, dropWhile isPathSeparator pth0)
             else (p0, pth0)
       ep   = tryCompileWith (unCOpts o) p
       ep'  = tryCompileWith (unCOpts o) ("./" ++ p)
       pat  = fromRight ep
       pat' = fromRight ep'
       pth' = "./" ++ pth
    in and [ isRight ep, isRight ep'
           , ( all (uncurry (==)) . (zip`ap`tail) $
                  [ match pat  pth
                  , match pat  pth'
                  , match pat' pth
                  , match pat' pth'
                  ]
             ) || null p
           ]

-- [/] shouldn't match anything
prop_match2 = not . match (compile "[/]")  . take 1 . unP

-- [!/] is like ?
prop_match3 p_ =
   let p = unP p_
       ~(x:_) = p
    in not (null p || isPathSeparator x || isExtSeparator x)
       ==> match (compile "[!/]") [x]

-- Anything should match itself, when compiled with everything disabled.
prop_match4 ps_ =
   let ps = unPS ps_
       noOpts = CompOptions { characterClasses = False
                            , characterRanges = False
                            , numberRanges = False
                            , wildcards = False
                            , recursiveWildcards = False
                            , pathSepInRanges = False
                            , errorRecovery = True
                            }
    in match (compileWith noOpts ps) ps
