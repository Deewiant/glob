-- File created: 2008-10-16 16:16:06

module Tests.Matcher (tests) where

import Control.Monad (ap)
import Test.Framework
import Test.Framework.Providers.QuickCheck

import System.FilePath.Glob.Compile
import System.FilePath.Glob.Match

import Tests.Base

tests =
   [ testGroup "Matcher"
       [ testProperty "match-1" prop_match1
       ]
   ]

-- ./foo should be equivalent to foo in both path and pattern
prop_match1 p s =
   let ep   = tryCompile (unPS p)
       ep'  = tryCompile ("./" ++ unPS p)
       pat  = fromRight ep
       pat' = fromRight ep'
       pth  = unP s
       pth' = "./" ++ pth
    in and [ isRight ep, isRight ep'
           , ( all (uncurry (==)) . (zip`ap`tail) $
                  [ match pat  pth
                  , match pat  pth' 
                  , match pat' pth
                  , match pat' pth'
                  ]
             ) || null (unPS p)
           ]
