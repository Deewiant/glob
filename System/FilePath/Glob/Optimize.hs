-- File created: 2008-10-10 13:29:17

module System.FilePath.Glob.Optimize (optimize) where

import Data.List (find, sortBy)

import System.FilePath.Glob.Base
import System.FilePath.Glob.Utils
   ( isLeft, fromLeft
   , increasingSeq
   , addToRange, overlap)

optimize :: Pattern -> Pattern
optimize = liftP (go . pre)
 where
   -- ./ at beginning -> nothing
   pre (ExtSeparator:PathSeparator:xs) = pre xs
   pre                             xs  = xs

   go [] = []
   go (CharRange r : xs) = CharRange (optimizeCharRange r) : go xs

   -- Literals to LongLiteral
   go (x:y:xs) | isLiteral x && isLiteral y =
      let (ls,rest) = span isLiteral xs
       in (:go rest) $
            LongLiteral (length ls + 2)
                        (foldr (\(Literal a) -> (a:)) [] (x:y:ls))

   -- /./ -> /
   go (PathSeparator:ExtSeparator:xs@(PathSeparator:_)) = go xs

   go (x:xs) =
      case find ($x) compressors of
           Just c  -> x : go (dropWhile c xs)
           Nothing -> x : go xs

   compressors = [isStar, isSlash, isStarSlash, isAnyNumber]

   isLiteral   (Literal _)                 = True
   isLiteral   _                           = False
   isStar      AnyNonPathSeparator         = True
   isStar      _                           = False
   isSlash     PathSeparator               = True
   isSlash     _                           = False
   isStarSlash AnyDirectory                = True
   isStarSlash _                           = False
   isAnyNumber (OpenRange Nothing Nothing) = True
   isAnyNumber _                           = False

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
                 let (ls,rest)        = span isLeft xs -- start from y
                     (catable,others) = increasingSeq (map fromLeft ls)
                     range            = (c, head catable)

                  in -- three (or more) Lefts make a Right
                     if null catable || null (tail catable)
                        then x : y : go ys
                        -- [abcd] -> [a-d]
                        else go$ Right range : map Left others ++ rest

              | otherwise -> x : go xs

           Right r : ys ->
              case addToRange r c of
                   -- [da-c] -> [a-d]
                   Just r' -> go$ Right r' : ys
                   Nothing -> x : go xs

   go (x@(Right r) : xs) =
      case xs of
           [] -> [x]
           Left c : ys ->
              case addToRange r c of
                   -- [a-cd] -> [a-d]
                   Just r' -> go$ Right r' : ys
                   Nothing -> x : go xs

           Right r' : ys ->
              case overlap r r' of
                   -- [a-cb-d] -> [a-d]
                   Just o  -> go$ Right o : ys
                   Nothing -> x : go xs

sortCharRange :: [Either Char (Char,Char)] -> [Either Char (Char,Char)]
sortCharRange = sortBy cmp
 where
   cmp (Left   a)    (Left   b)    = compare a b
   cmp (Left   a)    (Right (b,_)) = compare a b
   cmp (Right (a,_)) (Left   b)    = compare a b
   cmp (Right (a,_)) (Right (b,_)) = compare a b
