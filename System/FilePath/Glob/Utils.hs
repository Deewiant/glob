-- File created: 2008-10-10 13:40:35

module System.FilePath.Glob.Utils where

import Control.Exception (assert, handle, IOException)
import Data.List         ((\\), tails)
import System.Directory  (doesDirectoryExist, getDirectoryContents)
import System.FilePath   ((</>), joinPath, splitPath)
import System.IO.Unsafe  (unsafeInterleaveIO)

inRange :: Ord a => (a,a) -> a -> Bool
inRange (a,b) c = assert (b >= a) $ c >= a && c <= b

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
-- If x = fst (increasingSeq (a:xs)), then
-- x == reverse [a .. head x]
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

pathParts :: FilePath -> [FilePath]
pathParts = map joinPath . tails . splitPath

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents dir = handle (\e->const (return []) (e::IOException)) $ do
   raw <- getDirectoryContents dir

   let entries    = raw \\ [".",".."]
       absEntries =
          if dir == "."
             then entries
             else map (dir </>) entries

   (dirs,files) <- partitionM doesDirectoryExist absEntries

   subs <- unsafeInterleaveIO . mapM getRecursiveContents $ dirs

   return$ dir : files ++ concat subs

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ []     = return ([],[])
partitionM p (x:xs) = do
   ~(ts,fs) <- partitionM p xs
   b <- p x
   return $ if b
               then (x:ts, fs)
               else (ts, x:fs)
