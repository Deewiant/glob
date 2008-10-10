-- File created: 2008-10-10 13:29:17

module System.FilePath.Glob.Optimize (optimize) where

import Control.Exception (assert)
import Data.List (sortBy)

import System.FilePath.Glob.Base
import System.FilePath.Glob.Utils

optimize :: Pattern -> Pattern
optimize = liftP go
 where
   go [] = []
   go (PathSeparator:PathSeparator:xs) = PathSeparator : go xs
   go (CharRange r : xs) = CharRange (optimizeCharRange r) : go xs
   go (x:y:xs) | isLiteral x && isLiteral y =
      let (ls,rest) = span isLiteral xs
       in (:go rest) $
            LongLiteral (length ls + 2)
                        (foldr (\(Literal a) -> (a:)) [] (x:y:ls))
   go (x:xs) = x : go xs

   isLiteral (Literal _) = True
   isLiteral _           = False

optimizeCharRange :: [Either Char (Char,Char)] -> [Either Char (Char,Char)]
optimizeCharRange = go . sortCharRange
 where
   go [] = []

   go (x@(Left c) : xs) =
      case xs of
           [] -> [x]
           y@(Left d) : ys
              -- [aaaaa] -> [a]
              | c == d      -> go$ Left c : ys
              | d == succ c ->
                 let (ls,rest)        = span isLeft ys
                     (catable,others) = increasingSeq (map fromLeft ls)
                     range            = (c, head catable)
                  in if null catable
                        then x : y : go ys
                        -- [abcd] -> [a-d]
                        else go$ Right range : map Left others ++ rest

              | otherwise -> x : go xs

           Right (a,b) : ys ->
              case addToRange (a,b) c of
                   -- [da-c] -> [a-d]
                   Just r  -> go$ Right r : ys
                   Nothing -> x : go xs

   go (x@(Right (a,b)) : xs) =
      case xs of
           [] -> [x]
           Left c : ys ->
              case addToRange (a,b) c of
                   -- [a-cd] -> [a-d]
                   Just r  -> go$ Right r : ys --
                   Nothing -> x : go xs

           Right y : ys ->
              case overlap (a,b) y of
                   -- [a-cb-d] -> [a-d]
                   Just o  -> go$ Right o:ys
                   Nothing -> x : go xs

sortCharRange :: [Either Char (Char,Char)] -> [Either Char (Char,Char)]
sortCharRange = sortBy cmp
 where
   cmp (Left   a)    (Left   b)    = compare a b
   cmp (Left   a)    (Right (b,_)) = compare a b
   cmp (Right (a,_)) (Left   b)    = compare a b
   cmp (Right (a,_)) (Right (b,_)) = compare a b

-- returns Just (a range which covers both given ranges) or Nothing if they are
-- disjoint.
--
-- Assumes that the ranges are in the correct order, i.e. (fst x < snd x).
overlap :: Ord a => (a,a) -> (a,a) -> Maybe (a,a)
overlap (a,b) (c,d) = assert (b >= a) $ assert (d >= c) $
   if b >= c
      then if b >= d
              then if a <= c
                      then Just (a,b)
                      else Just (c,b)
              else if a <= c
                      then Just (a,d)
                      else Just (c,d)
      else Nothing

addToRange :: (Ord a, Enum a) => (a,a) -> a -> Maybe (a,a)
addToRange (a,b) c
   | inRange (a,b) c = Just (a,b)
   | c == pred a     = Just (c,b)
   | c == succ b     = Just (a,c)
   | otherwise       = Nothing

-- fst of result is in reverse order so that:
--
-- increasingSeq (a:xs) == [a .. head (increasingSeq (a:xs))]
increasingSeq :: (Eq a, Enum a) => [a] -> ([a],[a])
increasingSeq []     = ([],[])
increasingSeq (x:xs) = go [x] xs
 where
   go is       []     = (is,[])
   go is@(i:_) (y:ys) =
      if y == succ i
         then go (y:is) ys
         else (is, y:ys)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _        = error "fromLeft :: Right"

(-->) :: Bool -> Bool -> Bool
a --> b = not a || b

prop_overlapperLosesNoInfo (a1,b1) (a2,b2) c =
   let r1 = if b1 > a1 then (a1,b1) else (b1,a1)
       r2 = if b2 > a2 then (a2,b2) else (b2,a2)
       _  = c :: Int
    in case overlap r1 r2 of

        -- if the ranges don't overlap, nothing should be in both ranges
        Nothing -> not (inRange r1 c && inRange r2 c)

        -- if they do and something is in a range, it should be in the
        -- overlapped one as well
        Just o  -> (inRange r1 c --> inRange o c) &&
                   (inRange r2 c --> inRange o c)
