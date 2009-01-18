-- File created: 2008-10-10 16:28:53

module Tests.Utils (tests) where

import Data.Maybe
import Test.Framework
import Test.Framework.Providers.QuickCheck
import Test.QuickCheck

import System.FilePath.Glob.Utils

import Utils

tests = testGroup "Utils"
   [ testProperty "overlapperLosesNoInfo" prop_overlapperLosesNoInfo
   , testProperty "increasingSeq"         prop_increasingSeq
   , testProperty "addToRange"            prop_addToRange
   ]

validateRange (a,b) = if b > a then (a,b) else (b,a)

prop_overlapperLosesNoInfo x1 x2 c =
   let r1 = validateRange x1
       r2 = validateRange x2
       _  = c :: Float
    in case overlap r1 r2 of

        -- if the ranges don't overlap, nothing should be in both ranges
        Nothing -> not (inRange r1 c && inRange r2 c)

        -- if they do and something is in a range, it should be in the
        -- overlapped one as well
        Just o  -> (inRange r1 c --> inRange o c) &&
                   (inRange r2 c --> inRange o c)

prop_increasingSeq a xs =
   let s = fst . increasingSeq $ a:xs
    in s == reverse [a :: Float .. head s]

prop_addToRange x c =
   let r  = validateRange x
       r' = addToRange r c
    in isJust r' ==> inRange (fromJust r') (c :: Float)
