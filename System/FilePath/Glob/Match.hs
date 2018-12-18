-- File created: 2008-10-10 13:29:03

{-# LANGUAGE CPP #-}

module System.FilePath.Glob.Match (match, matchWith) where

import Control.Exception (assert)
import Data.Char         (isDigit, toLower, toUpper)
import Data.List         (elemIndex)
import Data.Maybe        (fromMaybe)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid       (mappend)
#endif
import System.FilePath   (isPathSeparator, isExtSeparator)

import System.FilePath.Glob.Base  ( Pattern(..), Token(..)
                                  , MatchOptions(..), matchDefault
                                  , isLiteral, tokToLower
                                  )
import System.FilePath.Glob.Utils (dropLeadingZeroes, inRange, pathParts)

-- |Matches the given 'Pattern' against the given 'FilePath', returning 'True'
-- if the pattern matches and 'False' otherwise.
match :: Pattern -> FilePath -> Bool
match = matchWith matchDefault

-- |Like 'match', but applies the given 'MatchOptions' instead of the defaults.
matchWith :: MatchOptions -> Pattern -> FilePath -> Bool
matchWith opts p f = begMatch opts (lcPat $ unPattern p) (lcPath f)
 where
   lcPath = if ignoreCase opts then map    toLower else id
   lcPat  = if ignoreCase opts then map tokToLower else id

-- begMatch takes care of some things at the beginning of a pattern or after /:
--    - . needs to be matched explicitly
--    - ./foo is equivalent to foo (for any number of /)
--
-- .*/foo still needs to match ./foo though, and it won't match plain foo;
-- special case that one
--
-- and .**/foo should /not/ match ../foo; more special casing
--
-- (All of the above is modulo options, of course)
begMatch, match' :: MatchOptions -> [Token] -> FilePath -> Bool
begMatch _ (Literal '.' : AnyDirectory : _) (x:y:_)
   | isExtSeparator x && isExtSeparator y = False

begMatch opts (Literal '.' : PathSeparator : pat) s | ignoreDotSlash opts =
   begMatch opts (dropWhile isSlash pat) (dropDotSlash s)
 where
   isSlash PathSeparator = True
   isSlash _             = False

   dropDotSlash (x:y:ys) | isExtSeparator x && isPathSeparator y =
      dropWhile isPathSeparator ys
   dropDotSlash xs = xs

begMatch opts pat (x:y:s)
   | dotSlash && dotStarSlash        = match' opts pat' s
   | ignoreDotSlash opts && dotSlash =
        begMatch opts pat (dropWhile isPathSeparator s)
 where
   dotSlash = isExtSeparator x && isPathSeparator y
   (dotStarSlash, pat') =
      case pat of
        Literal '.': AnyNonPathSeparator : PathSeparator : rest -> (True, rest)
        _                                                       -> (False, pat)

begMatch opts pat (e:_)
   | isExtSeparator e
     && not (matchDotsImplicitly opts)
     && not (isLiteral . Pattern $ take 1 pat) = False

begMatch opts pat s = match' opts pat s

match' _ []                        s  = null s
match' _ (AnyNonPathSeparator:s)   "" = null s
match' _ _                         "" = False
match' o (Literal l       :xs) (c:cs) = l == c && match' o xs cs
match' o (NonPathSeparator:xs) (c:cs) =
   not (isPathSeparator c) && match' o xs cs

match' o (PathSeparator   :xs) (c:cs) =
   isPathSeparator c && begMatch o (dropWhile (== PathSeparator) xs)
                                   (dropWhile isPathSeparator cs)

match' o (CharRange b rng :xs) (c:cs) =
   let rangeMatch r =
          either (== c) (`inRange` c) r ||
             -- See comment near Base.tokToLower for an explanation of why we
             -- do this
             ignoreCase o && either (== toUpper c) (`inRange` toUpper c) r
    in not (isPathSeparator c) &&
       any rangeMatch rng == b &&
       match' o xs cs

match' o (OpenRange lo hi :xs) path =
   let getNumChoices n =
          tail . takeWhile (not.null.snd) . map (`splitAt` n) $ [0..]
       (lzNum,cs) = span isDigit path
       num        = dropLeadingZeroes lzNum
       numChoices = getNumChoices num
       zeroChoices = takeWhile (all (=='0') . fst) (getNumChoices lzNum)
    in -- null lzNum means no digits: definitely not a match
       not (null lzNum) &&
          -- So, given the path "00123foo" what we've got is:
          --    lzNum       = "00123"
          --    cs          = "foo"
          --    num         = "123"
          --    numChoices  = [("1","23"),("12","3")]
          --    zeroChoices = [("0", "0123"), ("00", "123")]
          --
          -- We want to try matching x against each of 123, 12, and 1.
          -- 12 and 1 are in numChoices already, but we need to add (num,"")
          -- manually.
          --
          -- It's also possible that we only want to match the zeroes. Handle
          -- that separately since inOpenRange doesn't like leading zeroes.
          (any (\(n,rest) -> inOpenRange lo hi n && match' o xs (rest ++ cs))
               ((num,"") : numChoices)
           || (not (null zeroChoices) && inOpenRange lo hi "0"
               && any (\(_,rest) -> match' o xs (rest ++ cs)) zeroChoices))

match' o again@(AnyNonPathSeparator:xs) path@(c:cs) =
   match' o xs path || (not (isPathSeparator c) && match' o again cs)

match' o (AnyDirectory:xs) path =
   if not (matchDotsImplicitly o)
      --  **/ shouldn't match foo/.bar, so check that remaining bits don't
      -- start with .
      then all (not.isExtSeparator.head) matchedDirs && hasMatch
      else hasMatch
 where parts   = pathParts (dropWhile isPathSeparator path)
       matches = map (match' o xs) parts
       hasMatch = or matches
       matchIndex = fromMaybe 0 (elemIndex True matches)
       (matchedDirs, _) = splitAt matchIndex parts

match' o (LongLiteral len s:xs) path =
   let (pre,cs) = splitAt len path
    in pre == s && match' o xs cs

match' _ (Unmatchable:_) _ = False
match' _ (ExtSeparator:_) _ = error "ExtSeparator survived optimization?"

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
