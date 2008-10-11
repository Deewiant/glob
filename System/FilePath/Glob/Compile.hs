-- File created: 2008-10-10 13:29:13

module System.FilePath.Glob.Compile (compile, decompile, tokenize) where

import Control.Monad.Error ()
import Numeric         (readDec)
import System.FilePath
   ( isPathSeparator, pathSeparator
   ,  isExtSeparator,  extSeparator
   )

import System.FilePath.Glob.Base
import System.FilePath.Glob.Optimize (optimize)

compile :: String -> Either String Pattern
compile = fmap optimize . tokenize

decompile :: Pattern -> String
decompile = concatMap stringify . unPattern
 where
   stringify (Literal c)         = [c]
   stringify ExtSeparator        = [ extSeparator]
   stringify PathSeparator       = [pathSeparator]
   stringify NonPathSeparator    = "?"
   stringify AnyNonPathSeparator = "*"
   stringify AnyDirectory        = "**/"
   stringify (LongLiteral _ s)   = s
   stringify (CharRange r)       =
      '[' : concatMap (either (:[]) (\(a,b) -> [a,'-',b])) r ++ "]"
   stringify (OpenRange a b)     =
      '<' : maybe "" show a ++ "-" ++
            maybe "" show b ++ ">"

tokenize :: String -> Either String Pattern
tokenize = fmap Pattern . sequence . go
 where
   err s = [Left s]

   go :: String -> [Either String Token]
   go [] = []
   go ('?':cs) = Right NonPathSeparator : go cs
   go ('*':cs) =
      case cs of
           '*':ys@(p:xs) | isPathSeparator p ->
                            Right AnyDirectory        : go xs
                         | otherwise         ->
                            -- can eat the second * since * and ** are
                            -- equivalent
                            Right AnyNonPathSeparator : go ys
           _             -> Right AnyNonPathSeparator : go cs
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

openRange :: String -> Either String Token
openRange ['-']   = Right $ OpenRange Nothing Nothing
openRange ('-':s) =
   case readDec s of
        [(b,"")] -> Right $ OpenRange Nothing (Just b)
        _        -> Left $ "compile :: bad <>, expected number, got " ++ s
openRange s =
   case readDec s of
        [(a,"-")]    -> Right $ OpenRange (Just a) Nothing
        [(a,'-':s')] ->
           case readDec s' of
                [(b,"")] ->
                   case compare a b of
                        LT -> Right $ OpenRange (Just a) (Just b)
                        GT -> Right $ OpenRange (Just b) (Just a)
                        EQ -> let l = show a
                               in Right $ LongLiteral (length l) l
                _ -> Left $ "compile :: bad <>, expected number, got " ++ s'
        _ -> Left $ "compile :: bad <>, expected number followed by - in " ++ s

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
