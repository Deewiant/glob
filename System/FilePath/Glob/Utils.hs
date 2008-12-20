{-# LANGUAGE CPP #-}
-- File created: 2008-10-10 13:40:35

module System.FilePath.Glob.Utils
   ( isLeft, fromLeft
   , increasingSeq
   , addToRange, inRange, overlap
   , dropLeadingZeroes
   , pathParts
   , nubOrd
   , partitionDL
   , getRecursiveContents
   ) where

import Control.Monad    (foldM)
import Data.List        ((\\))
import qualified Data.DList as DL
import Data.DList       (DList)
import qualified Data.Set as Set
import System.Directory (getDirectoryContents)
import System.FilePath  ((</>), isPathSeparator, dropDrive)
import System.IO.Unsafe (unsafeInterleaveIO)

#if mingw32_HOST_OS
import Data.Bits          ((.&.))
import System.Win32.Types (withTString)
import System.Win32.File  (c_GetFileAttributes, fILE_ATTRIBUTE_DIRECTORY)
#else
import Foreign.C.String      (withCString)
import Foreign.Marshal.Alloc (allocaBytes)
import System.FilePath
   (isDrive, dropTrailingPathSeparator, addTrailingPathSeparator)
import System.Posix.Internals (sizeof_stat, lstat, s_isdir, st_mode)
#endif

inRange :: Ord a => (a,a) -> a -> Bool
inRange (a,b) c = c >= a && c <= b

-- returns Just (a range which covers both given ranges) or Nothing if they are
-- disjoint.
--
-- Assumes that the ranges are in the correct order, i.e. (fst x < snd x).
overlap :: Ord a => (a,a) -> (a,a) -> Maybe (a,a)
overlap (a,b) (c,d) =
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
   go _ _ = error "Glob.increasingSeq :: internal error"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

fromLeft :: Either a b -> a
fromLeft (Left x) = x
fromLeft _        = error "fromLeft :: Right"

dropLeadingZeroes :: String -> String
dropLeadingZeroes s =
   let x = dropWhile (=='0') s
    in if null x then "0" else x

-- foo/bar/baz -> [foo/bar/baz,bar/baz,baz]
pathParts :: FilePath -> [FilePath]
pathParts p = p : let d = dropDrive p
                   in if null d || d == p
                         then     f d
                         else d : f d
 where
   f []  = []
   f (x:xs@(y:_)) | isPathSeparator x && isPathSeparator y = f xs
   f (x:xs) =
      if isPathSeparator x
         then xs : f xs
         else      f xs

-- Significantly speedier than System.Directory.doesDirectoryExist.
doesDirectoryExist :: FilePath -> IO Bool
#if mingw32_HOST_OS
-- This one allocates more memory since it has to do a UTF-16 conversion, but
-- that can't really be helped: the below version is locale-dependent.
doesDirectoryExist = flip withTString $ \s -> do
   a <- c_GetFileAttributes s
   return (a /= 0xffffffff && a.&.fILE_ATTRIBUTE_DIRECTORY /= 0)
#else
doesDirectoryExist s =
   allocaBytes sizeof_stat $ \p ->
      withCString
         (if isDrive s
             then addTrailingPathSeparator s
             else dropTrailingPathSeparator s)
         $ \c -> do
            st <- lstat c p
            if st == 0
               then fmap s_isdir (st_mode p)
               else return False
#endif

getRecursiveContents :: FilePath -> IO (DList FilePath)
getRecursiveContents dir =
   flip Prelude.catch (\_ -> return $ DL.singleton dir) $ do

      raw <- getDirectoryContents dir

      let entries = map (dir </>) (raw \\ [".",".."])
      (dirs,files) <- partitionM doesDirectoryExist entries

      subs <- unsafeInterleaveIO . mapM getRecursiveContents $ dirs

      return$ DL.cons dir (DL.fromList files `DL.append` DL.concat subs)

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p_ = foldM (f p_) ([],[])
 where
   f p (ts,fs) x = p x >>= \b ->
      if b
         then return (x:ts, fs)
         else return (ts, x:fs)

partitionDL :: (a -> Bool) -> DList a -> (DList a, DList a)
partitionDL p_ = DL.foldr (f p_) (DL.empty,DL.empty)
 where
   f p x (ts,fs) =
      if p x
         then (DL.cons x ts, fs)
         else (ts, DL.cons x fs)

nubOrd :: Ord a => [a] -> [a]
nubOrd = go Set.empty
 where
   go _ [] = []
   go set (x:xs) =
      if Set.member x set
         then go set xs
         else x : go (Set.insert x set) xs
