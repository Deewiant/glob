-- File created: 16:28:53

module Utils
   ( prop_overlapperLosesNoInfo
   , prop_increasingSeq1
   ) where

import System.FilePath.Glob.Utils

(-->) :: Bool -> Bool -> Bool
a --> b = not a || b

prop_overlapperLosesNoInfo (a1,b1) (a2,b2) c =
   let r1 = if b1 > a1 then (a1,b1) else (b1,a1)
       r2 = if b2 > a2 then (a2,b2) else (b2,a2)
       _  = c :: Float
    in case overlap r1 r2 of

        -- if the ranges don't overlap, nothing should be in both ranges
        Nothing -> not (inRange r1 c && inRange r2 c)

        -- if they do and something is in a range, it should be in the
        -- overlapped one as well
        Just o  -> (inRange r1 c --> inRange o c) &&
                   (inRange r2 c --> inRange o c)

prop_increasingSeq1 a xs =
   let s = fst . increasingSeq $ a:xs
    in s == reverse [a :: Float .. head s]
