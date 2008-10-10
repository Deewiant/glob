-- File created: 2008-10-10 13:29:13

module System.FilePath.Glob.Compile (compile, decompile) where

import Numeric         (readDec)
import System.FilePath
   ( isPathSeparator, pathSeparator
   ,  isExtSeparator,  extSeparator
   )

import System.FilePath.Glob.Base
import System.FilePath.Glob.Optimize (optimize)

compile :: String -> Pattern
compile = optimize . tokenize

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
      '<' : maybe "" show a ++
            maybe "" show b ++ ">"

tokenize :: String -> Pattern
tokenize = Pattern . go
 where
   go [] = []
   go ('?':cs) = NonPathSeparator : go cs
   go ('*':cs) =
      case cs of
           ('*':p:xs) | isPathSeparator p -> AnyDirectory        : go xs
                      | otherwise         -> AnyNonPathSeparator : go xs
           _                              -> AnyNonPathSeparator : go cs
   go ('[':cs) =
      let (range, rest) = break (==']') cs
       in if null rest
             then error "compile :: unclosed [] in pattern"
             else if null range
                     then let (range', rest') = break (==']') (tail rest)
                           in if null rest'
                                 then error "compile :: unclosed [] in pattern"
                                 else charRange range' : go (tail rest')
                     else charRange range : go (tail rest)
   go ('<':cs) =
      let (range, rest) = break (=='>') cs
       in if null rest
             then error "compile :: unclosed <> in pattern"
             else openRange range : go (tail rest)
   go (c:cs)
      | isPathSeparator c = PathSeparator : go cs
      | isExtSeparator  c =  ExtSeparator : go cs
      | otherwise         = Literal c     : go cs

openRange :: String -> Token
openRange ['-']   = OpenRange Nothing Nothing
openRange ('-':s) =
   case readDec s of
        [(b,"")] -> OpenRange Nothing (Just b)
        _        -> error "compile :: bad <>, expected only number after -"
openRange s =
   case readDec s of
        [(a,"-")]    -> OpenRange (Just a) Nothing
        [(a,'-':s')] ->
           case readDec s' of
                [(b,"")] ->
                   case compare a b of
                        LT -> OpenRange (Just a) (Just b)
                        GT -> OpenRange (Just b) (Just a)
                        EQ -> let l = show a in LongLiteral (length l) l
                _ -> error "compile :: bad <>, expected only number after -"
        _ -> error "compile :: bad <>, expected number followed by -"

charRange :: String -> Token
charRange = CharRange . go
 where
   go [] = []
   go (a:'-':b:cs) = (: go cs) $
      case compare b a of
           GT -> Right (a,b)
           LT -> Right (b,a)
           EQ -> Left b
   go (c:cs)       = Left c : go cs
