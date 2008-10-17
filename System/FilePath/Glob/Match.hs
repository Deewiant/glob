-- File created: 2008-10-10 13:29:03

module System.FilePath.Glob.Match (match) where

import Control.Exception (assert)
import Data.Char         (isDigit)
import Data.Monoid       (mappend)
import System.FilePath   (isPathSeparator, isExtSeparator)

import System.FilePath.Glob.Base
import System.FilePath.Glob.Utils (dropLeadingZeroes, inRange, pathParts)

-- |Matches the given 'Pattern' against the given 'FilePath', returning 'True'
-- if the pattern matches and 'False' otherwise.
match :: Pattern -> FilePath -> Bool
match _         "." = False
match _        ".." = False
match (Pattern p) s = begMatch p s

-- begMatch takes care of some things at the beginning of a pattern or after /:
--    - . needs to be matched explicitly
--    - ./foo is equivalent to foo
begMatch, match' :: [Token] -> FilePath -> Bool
begMatch (ExtSeparator:PathSeparator:pat) s                  = begMatch pat s
begMatch pat (x:y:s) | isExtSeparator x && isPathSeparator y = begMatch pat s
begMatch pat s =
   if not (null s) && isExtSeparator (head s)
      then case pat of
                ExtSeparator:pat' -> match' pat' (tail s)
                _                 -> False
      else match' pat s

match' []                        s  = null s
match' (AnyNonPathSeparator:_)   "" = True
match' _                         "" = False
match' (Literal l       :xs) (c:cs) =                 l == c  && match'   xs cs
match' ( ExtSeparator   :xs) (c:cs) =       isExtSeparator c  && match'   xs cs
match' (PathSeparator   :xs) (c:cs) =      isPathSeparator c  && begMatch xs cs
match' (NonPathSeparator:xs) (c:cs) = not (isPathSeparator c) && match'   xs cs

match' (CharRange _     :_ ) (c:_)
   | isExtSeparator c || isPathSeparator c
      = False

match' (CharRange range :xs) (c:cs) =
   any (either (== c) (`inRange` c)) range && match' xs cs

match' (OpenRange lo hi :xs) path =
   let
      (lzNum,cs) = span isDigit path
      num        = dropLeadingZeroes lzNum
      numChoices =
         tail . takeWhile (not.null.snd) . map (flip splitAt num) $ [0..]
    in if null lzNum
          then False -- no digits
          else
            -- So, given the path "123foo" what we've got is:
            --    cs         = "foo"
            --    num        = "123"
            --    numChoices = [("1","23"),("12","3")]
            --
            -- We want to try matching x against each of 123, 12, and 1.
            -- 12 and 1 are in numChoices already, but we need to add (num,"")
            -- manually.
            any (\(n,rest) -> inOpenRange lo hi n && match' xs (rest ++ cs))
                ((num,"") : numChoices)

match' again@(AnyNonPathSeparator:xs) path@(c:cs) =
   if isPathSeparator c
      then match' xs path

      -- AnyNonPathSeparator [] is not a separate case since we don't want
      -- anything except "" to match ""; instead, check for a null tail here
      else null cs || match' again cs

match' again@(AnyDirectory:xs) path =
   let parts   = pathParts path
       matches = any (match' xs) parts || any (match' again) (tail parts)
    in if null xs
          --  **/ shouldn't match foo/.bar, so check that remaining bits don't
          -- start with .
          then all (not.isExtSeparator.head) (init parts) && matches
          else matches

match' (LongLiteral len s:xs) path =
   let (pre,cs) = splitAt len path
    in pre == s && match' xs cs

-- Does the actual open range matching: finds whether the third parameter
-- is between the first two or not.
--
-- It does this by keeping track of the Ordering so far (e.g. having
-- looked at "12" and "34" the Ordering of the two would be LT: 12 < 34)
-- and aborting if a String "runs out": a longer string is automatically
-- greater.
--
-- Assumes that the input strings contain only digits, and no leading zeroes.
inOpenRange :: Maybe String -> Maybe String -> String -> Bool
inOpenRange l_ h_ s_ = assert (all isDigit s_) $ go l_ h_ s_ EQ EQ
 where
   go Nothing      Nothing   _     _ _  = True  -- no bounds
   go (Just [])    _         []    LT _ = False --  lesser than lower bound
   go _            (Just []) _     _ GT = False -- greater than upper bound
   go _            (Just []) (_:_) _ _  = False --  longer than upper bound
   go (Just (_:_)) _         []    _ _  = False -- shorter than lower bound
   go _            _         []    _ _  = True

   go (Just (l:ls)) (Just (h:hs)) (c:cs) ordl ordh =
      let ordl' = ordl `mappend` compare c l
          ordh' = ordh `mappend` compare c h
       in go (Just ls) (Just hs) cs ordl' ordh'

   go Nothing (Just (h:hs)) (c:cs) _ ordh =
      let ordh' = ordh `mappend` compare c h
       in go Nothing (Just hs) cs GT ordh'

   go (Just (l:ls)) Nothing (c:cs) ordl _ =
      let ordl' = ordl `mappend` compare c l
       in go (Just ls) Nothing cs ordl' LT

   -- lower bound is shorter: s is greater
   go (Just []) hi s _ ordh = go Nothing hi s GT ordh
