-- File created: 2008-10-10 13:29:13

module System.FilePath.Glob.Compile
   (    compile,    compileWith
   , tryCompile, tryCompileWith
   , tokenize
   ) where

import Control.Arrow               (first)
import Control.Monad.Error         (ErrorT, runErrorT, throwError)
import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import Data.Char                   (isDigit,isAlpha)
import System.FilePath             (isPathSeparator, isExtSeparator)

import System.FilePath.Glob.Base
import System.FilePath.Glob.Optimize (optimize)
import System.FilePath.Glob.Utils    (dropLeadingZeroes)

-- |Like 'tryCompile', but calls 'error' if an error results.
compile :: String -> Pattern
compile = compileWith compExtended

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
-- [@\<m-n>@] Matches any integer in the range m to n, inclusive. The range may
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
tryCompile = tryCompileWith compExtended

-- |Like 'compile', but recognizes operators according to the 'CompOptions'
-- given.
compileWith :: CompOptions -> String -> Pattern
compileWith opts = either error id . tryCompileWith opts

-- |Like 'tryCompile', but recognizes operators according to the 'CompOptions'
-- given.
tryCompileWith :: CompOptions -> String -> Either String Pattern
tryCompileWith opts = fmap optimize . tokenize opts

tokenize :: CompOptions -> String -> Either String Pattern
tokenize opts = fmap Pattern . sequence . go
 where
   err _ c cs | errorRecovery opts = Right (Literal c) : go cs
   err s _ _                       = [Left s]

   go :: String -> [Either String Token]
   go [] = []
   go ('?':cs) | wcs = Right NonPathSeparator : go cs
   go ('*':cs) | wcs =
      case cs of
           '*':p:xs | rwcs && isPathSeparator p
              -> Right AnyDirectory        : go xs
           _  -> Right AnyNonPathSeparator : go cs

   go ('[':cs) | crs = let (range,rest) = charRange opts cs
                        in case range of
                                Left s -> err s '[' cs
                                r      -> r : go rest

   go ('<':cs) | ors =
      let (range, rest) = break (=='>') cs
       in if null rest
             then err "compile :: unclosed <> in pattern" '<' cs
             else case openRange range of
                       Left s -> err s '<' cs
                       r      -> r : go (tail rest)
   go (c:cs)
      | isPathSeparator c = Right PathSeparator : go cs
      | isExtSeparator  c = Right  ExtSeparator : go cs
      | otherwise         = Right (Literal c)   : go cs

   wcs  = wildcards          opts
   rwcs = recursiveWildcards opts
   crs  = characterRanges    opts
   ors  = openRanges         opts

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

charRange :: CompOptions -> String -> (Either String Token, String)
charRange opts xs_ =
   case xs_ of
        (x:xs') | x `elem` "^!" -> first (fmap (CharRange False)) (start xs')
        _                       -> first (fmap (CharRange  True)) (start xs_)
 where
   start :: String -> (Either String CharRange, String)
   start (']':xs) = run $ char ']' xs
   start ('-':xs) = run $ char '-' xs
   start xs       = run $ go xs

   run :: ErrorT String (Writer CharRange) String
       -> (Either String CharRange, String)
   run m = case runWriter.runErrorT $ m of
                (Left   err,  _) -> (Left err, [])
                (Right rest, cs) -> (Right cs, rest)

   go :: String -> ErrorT String (Writer CharRange) String
   go ('[':':':xs) | characterClasses opts = readClass xs
   go (    ']':xs)                         = return xs
   go (      c:xs)                         = char c xs
   go []           = throwError "compile :: unclosed [] in pattern"

   char :: Char -> String -> ErrorT String (Writer CharRange) String
   char c ('-':x:xs) =
      if x == ']'
         then tell [Left c, Left '-'] >> return xs
         else tell [Right (c,x)]      >>     go xs

   char c xs = tell [Left c] >> go xs

   readClass :: String -> ErrorT String (Writer CharRange) String
   readClass xs = let (name,end) = span isAlpha xs
                   in case end of
                           ':':']':rest -> charClass name           >> go rest
                           _            -> tell [Left '[',Left ':'] >> go xs

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
           _        ->
              throwError ("compile :: unknown character class '" ++name++ "'")

   digit  = Right ('0','9')
   upper  = Right ('A','Z')
   lower  = Right ('a','z')
   punct  = map Right [('!','/'), (':','@'), ('[','`'), ('{','~')]
   blanks = [Left '\t',         Left ' ']
   spaces = [Right ('\t','\r'), Left ' ']
