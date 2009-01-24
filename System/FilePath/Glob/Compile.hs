-- File created: 2008-10-10 13:29:13

module System.FilePath.Glob.Compile
   ( compile, tryCompile
   , tokenize
   ) where

import Control.Monad.Error  (ErrorT, runErrorT, throwError)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Char            (isDigit,isAlpha)
import System.FilePath      (isPathSeparator, isExtSeparator)

import System.FilePath.Glob.Base
import System.FilePath.Glob.Optimize (optimize)
import System.FilePath.Glob.Utils    (dropLeadingZeroes)

-- |Like 'tryCompile', but calls 'error' if an error results.
compile :: String -> Pattern
compile = either error id . tryCompile

-- |Compiles a glob pattern from its textual representation into a 'Pattern'
-- object, giving an error message in a 'Left' if the pattern is erroneous.
--
-- For the most part, a character matches itself. Recognized operators are as
-- follows:
--
-- [@?@]      Matches any character except path separators.
--
-- [@*@]      Matches any number of characters except path separators,
--            including the empty string.
--
-- [@[..\]@]  Matches any of the enclosed characters. Ranges of characters can
--            be specified by separating the endpoints with a \'-'. \'-' or ']'
--            can be matched by including them as the first character(s) in the
--            list.
--
-- [@[^..\]@ or @[!..\]@] Like [..], but matches any character /not/ listed.
--
-- [@\<m-n>@]  Matches any integer in the range m to n, inclusive. The range may
--            be open-ended by leaving out either number: \"\<->\", for
--            instance, matches any integer.
--
-- [@**/@]    Matches any number of characters, including path separators,
--            excluding the empty string.
--
-- Note that path separators (typically @\'/\'@) have to be matched explicitly
-- or using the @**/@ pattern. In addition, extension separators (typically
-- @\'.\'@) have to be matched explicitly at the beginning of the pattern or
-- after any path separator.
--
-- If a system supports multiple path separators, any one of them will match
-- any of them. For instance, on Windows, @\'/\'@ will match itself as well as
-- @\'\\\'@.
--
-- Erroneous patterns include:
--
-- * An empty @[]@ or @[^]@ or @[!]@
--
-- * A @[@ or @\<@ without a matching @]@ or @>@
--
-- * A malformed @\<>@: e.g. nonnumeric characters or no hyphen
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
   go ('[':cs) = let (range,rest) = charRange cs in range:go rest
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

type CharRange = [Either Char (Char,Char)]

charRange :: String -> (Either String Token,String)
charRange xs_ =
   case xs_ of
        (x:xs') | x `elem` "^!" -> let (rest,cs) = start xs'
                                    in (fmap (CharRange False) cs,rest)
        _                       -> let (rest,cs) = start xs_
                                    in (fmap (CharRange True) cs,rest)
 where
   start :: String -> (String, Either String CharRange)
   start (']':xs) = run $ char ']' xs
   start ('-':xs) = run $ char '-' xs
   start xs       = run $ go xs

   run :: ErrorT String (Writer CharRange) String
       -> (String, Either String CharRange)
   run m = case runWriter $ runErrorT m of (Left s,_) -> ("",Left s)
                                           (Right rest,cs) -> (rest,Right cs)

   go :: String -> ErrorT String (Writer CharRange) String
   go []           = throwError "unclosed [] in pattern"
   go ('[':':':xs) = readClass xs
   go (    ']':xs) = return xs
   go (      c:xs) = char c xs

   readClass :: String -> ErrorT String (Writer CharRange) String
   readClass xs = let (name,end) = span isAlpha xs
                  in case end of
                       ':':']':rest -> charClass name >> go rest
                       _ -> tell [Left '[',Left ':'] >> go xs

   char :: Char -> String -> ErrorT String (Writer CharRange) String
   char f ('-':s:cs) | not $ s `elem` "[]" = tell [Right (f,s)] >> go cs
   char c xs = tell [Left c] >> go xs

   charClass :: String -> ErrorT String (Writer CharRange) ()
   charClass name =
      -- The POSIX classes
      --
      -- TODO: this is ASCII-only, not sure how this should be extended
      --       Unicode, or with a locale as input, or something else?
      case name of
           "alnum"  -> tell [digit,upper,lower]
           "alpha"  -> tell [upper,lower]
           "blank"  -> tell blanks
           "cntrl"  -> tell [Right ('\0','\x1f'), Left '\x7f']
           "digit"  -> tell [digit]
           "graph"  -> tell [Right ('!','~')]
           "lower"  -> tell [lower]
           "print"  -> tell [Right (' ','~')]
           "punct"  -> tell punct
           "space"  -> tell spaces
           "upper"  -> tell [upper]
           "xdigit" -> tell [digit, Right ('A','F'), Right ('a','f')]
           _        -> throwError $ "unknown character class: "++name

   digit  = Right ('0','9')
   upper  = Right ('A','Z')
   lower  = Right ('a','z')
   punct  = map Right [('!','/'), (':','@'), ('[','`'), ('{','~')]
   blanks = [Left '\t',         Left ' ']
   spaces = [Right ('\t','\r'), Left ' ']
