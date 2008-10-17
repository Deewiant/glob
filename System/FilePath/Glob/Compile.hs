-- File created: 2008-10-10 13:29:13

module System.FilePath.Glob.Compile
   ( compile, tryCompile
   , tokenize
   ) where

import Control.Monad.Error ()
import Data.Char           (isDigit)
import System.FilePath     (isPathSeparator, isExtSeparator)

import System.FilePath.Glob.Base
import System.FilePath.Glob.Optimize (optimize)
import System.FilePath.Glob.Utils    (dropLeadingZeroes)

compile :: String -> Pattern
compile = either error id . tryCompile

tryCompile :: String -> Either String Pattern
tryCompile = fmap optimize . tokenize

tokenize :: String -> Either String Pattern
tokenize = fmap Pattern . sequence . go
 where
   err s = [Left s]

   go :: String -> [Either String Token]
   go [] = []
   go ('?':cs) = Right NonPathSeparator : go cs
   go ('*':cs) =
      case cs of
           '*':p:xs | isPathSeparator p -> Right AnyDirectory        : go xs
           _                            -> Right AnyNonPathSeparator : go cs
   go ('[':cs) =
      let (range, rest) = break (==']') cs
       in if null rest
             then err "compile :: unclosed [] in pattern"
             else if null range
                     then let (range', rest') = break (==']') (tail rest)
                           in if null rest'
                                 then err "compile :: unclosed [] in pattern"
                                 else charRange range' : go (tail rest')
                     else charRange range : go (tail rest)
   go ('<':cs) =
      let (range, rest) = break (=='>') cs
       in if null rest
             then err "compile :: unclosed <> in pattern"
             else openRange range : go (tail rest)
   go (c:cs)
      | isPathSeparator c = Right PathSeparator : go cs
      | isExtSeparator  c = Right  ExtSeparator : go cs
      | otherwise         = Right (Literal c)   : go cs

-- <a-b> where a > b can never match anything; this is not considered an error
openRange :: String -> Either String Token
openRange ['-']   = Right $ OpenRange Nothing Nothing
openRange ('-':s) =
   case span isDigit s of
        (b,"") -> Right $ OpenRange Nothing (openRangeNum b)
        _      -> Left $ "compile :: bad <>, expected number, got " ++ s
openRange s =
   case span isDigit s of
        (a,"-")    -> Right $ OpenRange (openRangeNum a) Nothing
        (a,'-':s') ->
           case span isDigit s' of
                (b,"") -> Right $ OpenRange (openRangeNum a) (openRangeNum b)
                _ -> Left $ "compile :: bad <>, expected number, got " ++ s'
        _ -> Left $ "compile :: bad <>, expected number followed by - in " ++ s

openRangeNum :: String -> Maybe String
openRangeNum = Just . dropLeadingZeroes

charRange :: String -> Either String Token
charRange = Right . CharRange . go
 where
   go [] = []
   go (a:'-':b:cs) = (: go cs) $
      case compare b a of
           GT -> Right (a,b)
           LT -> Right (b,a)
           EQ -> Left b
   go (c:cs)       = Left c : go cs
