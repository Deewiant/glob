-- File created: 2008-10-16 16:16:06

module Tests.Matcher (tests) where

import Control.Monad (ap)
import Test.Framework
import Test.Framework.Providers.QuickCheck
import Test.QuickCheck ((==>))

import System.FilePath (isExtSeparator, isPathSeparator)
import System.FilePath.Glob.Base
import System.FilePath.Glob.Match

import Tests.Base

tests = testGroup "Matcher"
   [ testProperty "match-1" prop_match1
   , testProperty "match-2" prop_match2
   , testProperty "match-3" prop_match3
   ]

-- ./foo should be equivalent to foo in both path and pattern
-- ... but not for the pattern if it starts with /
prop_match1 o p_ s =
   let p    = dropWhile isPathSeparator (unPS p_)
       ep   = tryCompileWith (unCOpts o) p
       ep'  = tryCompileWith (unCOpts o) ("./" ++ p)
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
