-- File created: 2008-10-10 16:28:53

-- A bit of a misnomer: tests Utils but also has Utils for testing
module Utils
   ( prop_overlapperLosesNoInfo
   , prop_increasingSeq
   , prop_addToRange
   , fromRight, isRight
   ) where

import Data.Maybe
import Test.QuickCheck

import System.FilePath.Glob.Utils

fromRight (Right x) = x
fromRight _         = error "fromRight :: Left"

isRight (Right _) = True
isRight _         = False

a --> b = not a || b

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
