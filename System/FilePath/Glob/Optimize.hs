-- File created: 2008-10-10 13:29:17

module System.FilePath.Glob.Optimize (optimize) where

import Data.List (sortBy)

import System.FilePath.Glob.Base
import System.FilePath.Glob.Utils

optimize :: Pattern -> Pattern
optimize = liftP go
 where
   go [] = []
   go (CharRange r : xs) = CharRange (optimizeCharRange r) : go xs
   go (x:y:xs) | isLiteral x && isLiteral y =
      let (ls,rest) = span isLiteral xs
       in (:go rest) $
            LongLiteral (length ls + 2)
                        (foldr (\(Literal a) -> (a:)) [] (x:y:ls))

   go (x@AnyNonPathSeparator : xs) = x : go (dropWhile isStar xs)
   go (x@PathSeparator       : xs) = x : go (dropWhile isSlash xs)
   go (x:xs) = x : go xs

   isLiteral (Literal _) = True
   isLiteral _           = False

   isStar AnyNonPathSeparator = True
   isStar _                   = False

   isSlash PathSeparator = True
   isSlash _             = False

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
