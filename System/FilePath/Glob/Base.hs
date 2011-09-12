-- File created: 2008-10-10 13:29:26

{-# LANGUAGE CPP #-}

module System.FilePath.Glob.Base
   ( Token(..), Pattern(..)

   , CompOptions(..), MatchOptions(..)
   , compDefault, compPosix, matchDefault, matchPosix

   , decompile

   , compile
   , compileWith, tryCompileWith
   , tokenize -- for tests

   , optimize

   , liftP, tokToLower
   ) where

import Control.Arrow                     (first)
import Control.Monad.Trans.Class         (lift)
import Control.Monad.Trans.Error         (ErrorT, runErrorT, throwError)
import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import Control.Exception                 (assert)
import Data.Char                         (isDigit, isAlpha, toLower)
import Data.List                         (find, sortBy)
import Data.Maybe                        (fromMaybe)
import Data.Monoid                       (Monoid, mappend, mempty, mconcat)
import System.FilePath                   ( pathSeparator, extSeparator
                                         , isExtSeparator, isPathSeparator
                                         )

import System.FilePath.Glob.Utils ( dropLeadingZeroes
                                  , isLeft, fromLeft
                                  , increasingSeq
                                  , addToRange, overlap
                                  )

#if __GLASGOW_HASKELL__
import Text.Read (readPrec, lexP, parens, prec, Lexeme(Ident))
#endif

data Token
   -- primitives
   = Literal !Char
   | ExtSeparator                              --  .
   | PathSeparator                             --  /
   | NonPathSeparator                          --  ?
   | CharRange !Bool [Either Char (Char,Char)] --  []
   | OpenRange (Maybe String) (Maybe String)   --  <>
   | AnyNonPathSeparator                       --  *
   | AnyDirectory                              --  **/

   -- after optimization only
   | LongLiteral !Int String
   deriving (Eq)

-- Note: CharRanges aren't converted, because this is tricky in general.
-- Consider for instance [@-[], which includes the range A-Z. This would need
-- to become [@[a-z]: so essentially we'd need to either:
--
--    1) Have a list of ranges of uppercase Unicode. Check if our range
--       overlaps with any of them and if it does, take the non-overlapping
--       part and combine it with the toLower of the overlapping part.
--
--    2) Simply expand the entire range to a list and map toLower over it.
--
-- In either case we'd need to re-optimize the CharRange—we can't assume that
-- if the uppercase characters are consecutive, so are the lowercase.
--
-- 1) might be feasible if someone bothered to get the latest data.
--
-- 2) obviously isn't since you might have 'Right (minBound, maxBound)' in
-- there somewhere.
--
-- The current solution is to just check both the toUpper of the character and
-- the toLower.
tokToLower :: Token -> Token
tokToLower (Literal     c)   = Literal       (toLower c)
tokToLower (LongLiteral n s) = LongLiteral n (map toLower s)
tokToLower tok               = tok

-- |An abstract data type representing a compiled pattern.
--
-- Note that the 'Eq' instance cannot tell you whether two patterns behave in
-- the same way; only whether they compile to the same 'Pattern'. For instance,
-- @'compile' \"x\"@ and @'compile' \"[x]\"@ may or may not compare equal,
-- though a @'match'@ will behave the exact same way no matter which 'Pattern'
-- is used.
newtype Pattern = Pattern { unPattern :: [Token] } deriving (Eq)

liftP :: ([Token] -> [Token]) -> Pattern -> Pattern
liftP f (Pattern pat) = Pattern (f pat)

instance Show Token where
   show (Literal c)
      | c `elem` "*?[<" || isExtSeparator c
                            = ['[',c,']']
      | otherwise           = assert (not $ isPathSeparator c) [c]
   show ExtSeparator        = [ extSeparator]
   show PathSeparator       = [pathSeparator]
   show NonPathSeparator    = "?"
   show AnyNonPathSeparator = "*"
   show AnyDirectory        = "**/"
   show (LongLiteral _ s)   = concatMap (show . Literal) s
   show (OpenRange a b)     =
      '<' : fromMaybe "" a ++ "-" ++
            fromMaybe "" b ++ ">"

   -- We have to be careful here with ^ and ! lest [a!b] become [!ab]. So we
   -- just put them at the end.
   --
   -- Also, [^x-] was sorted and should not become [^-x].
   show (CharRange b r)     =
      let f = either (:[]) (\(x,y) -> [x,'-',y])
          (caret,exclamation,fs) =
             foldr (\c (ca,ex,ss) ->
                case c of
                     Left '^' -> ("^",ex,ss)
                     Left '!' -> (ca,"!",ss)
                     _        -> (ca,  ex,(f c ++) . ss)
                   )
                   ("", "", id)
                   r
          (beg,rest) = let s' = fs []
                           (x,y) = splitAt 1 s'
                           in if not b && x == "-"
                                 then (y,x)
                                 else (s',"")
       in concat [ "["
                 , if b then "" else "^"
                 , beg, caret, exclamation, rest
                 , "]"
                 ]

instance Show Pattern where
   showsPrec d p = showParen (d > 10) $
      showString "compile " . showsPrec (d+1) (decompile p)

instance Read Pattern where
#if __GLASGOW_HASKELL__
   readPrec = parens . prec 10 $ do
      Ident "compile" <- lexP
      fmap compile readPrec
#else
   readsPrec d = readParen (d > 10) $ \r -> do
      ("compile",string) <- lex r
      (xs,rest) <- readsPrec (d+1) string
      [(compile xs, rest)]
#endif

instance Monoid Pattern where
   mempty                          = Pattern []
   mappend (Pattern a) (Pattern b) = optimize . Pattern $ (a ++ b)
   mconcat                         = optimize . Pattern . concatMap unPattern

-- |Options which can be passed to the 'tryCompileWith' or 'compileWith'
-- functions: with these you can selectively toggle certain features at compile
-- time.
--
-- Note that some of these options depend on each other: classes can never
-- occur if ranges aren't allowed, for instance.

-- We could presumably put locale information in here, too.
data CompOptions = CompOptions
    { characterClasses   :: Bool -- ^Allow character classes, @[[:...:]]@.
    , characterRanges    :: Bool -- ^Allow character ranges, @[...]@.
    , numberRanges       :: Bool -- ^Allow open ranges, @\<...>@.
    , wildcards          :: Bool -- ^Allow wildcards, @*@ and @?@.
    , recursiveWildcards :: Bool -- ^Allow recursive wildcards, @**/@.

    , pathSepInRanges    :: Bool
      -- ^Allow path separators in character ranges.
      --
      -- If true, @a[/]b@ never matches anything (since character ranges can't
      -- match path separators); if false and 'errorRecovery' is enabled,
      -- @a[/]b@ matches itself, i.e. a file named @]b@ in the subdirectory
      -- @a[@.

    , errorRecovery      :: Bool
      -- ^If the input is invalid, recover by turning any invalid part into
      -- literals. For instance, with 'characterRanges' enabled, @[abc@ is an
      -- error by default (unclosed character range); with 'errorRecovery', the
      -- @[@ is turned into a literal match, as though 'characterRanges' were
      -- disabled.
    } deriving (Show,Read,Eq)

-- |The default set of compilation options: closest to the behaviour of the
-- @zsh@ shell, with 'errorRecovery' enabled.
--
-- All options are enabled.
compDefault :: CompOptions
compDefault = CompOptions
   { characterClasses   = True
   , characterRanges    = True
   , numberRanges       = True
   , wildcards          = True
   , recursiveWildcards = True
   , pathSepInRanges    = True
   , errorRecovery      = True
   }

-- |Options for POSIX-compliance, as described in @man 7 glob@.
--
-- 'numberRanges', 'recursiveWildcards', and 'pathSepInRanges' are disabled.
compPosix :: CompOptions
compPosix = CompOptions
   { characterClasses   = True
   , characterRanges    = True
   , numberRanges       = False
   , wildcards          = True
   , recursiveWildcards = False
   , pathSepInRanges    = False
   , errorRecovery      = True
   }

-- |Options which can be passed to the 'matchWith' or 'globDirWith' functions:
-- with these you can selectively toggle certain features at matching time.
data MatchOptions = MatchOptions
    { matchDotsImplicitly :: Bool
      -- ^Allow @*@, @?@, and @**/@ to match @.@ at the beginning of paths.

    , ignoreCase          :: Bool
      -- ^Case-independent matching.

    , ignoreDotSlash      :: Bool
      -- ^Treat @./@ as a no-op in both paths and patterns.
      --
      -- (Of course e.g. @../@ means something different and will not be
      -- ignored.)
    }

-- |The default set of execution options: closest to the behaviour of the @zsh@
-- shell.
--
-- Currently identical to 'matchPosix'.
matchDefault :: MatchOptions
matchDefault = matchPosix

-- |Options for POSIX-compliance, as described in @man 7 glob@.
--
-- 'ignoreDotSlash' is enabled, the rest are disabled.
matchPosix :: MatchOptions
matchPosix = MatchOptions
   { matchDotsImplicitly = False
   , ignoreCase          = False
   , ignoreDotSlash      = True
   }

-- |Decompiles a 'Pattern' object into its textual representation: essentially
-- the inverse of 'compile'.
--
-- Note, however, that due to internal optimization, @decompile . compile@ is
-- not the identity function. Instead, @compile . decompile@ is.
--
-- Be careful with 'CompOptions': 'decompile' always produces a 'String' which
-- can be passed to 'compile' to get back the same 'Pattern'. @compileWith
-- options . decompile@ is /not/ the identity function unless @options@ is
-- 'compDefault'.
decompile :: Pattern -> String
decompile = concatMap show . unPattern

------------------------------------------
-- COMPILATION
------------------------------------------


-- |Compiles a glob pattern from its textual representation into a 'Pattern'
-- object.
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
--            be specified by separating the endpoints with a @\'-'@. @\'-'@ or
--            @']'@ can be matched by including them as the first character(s)
--            in the list. Never matches path separators: @[\/]@ matches
--            nothing at all. Named character classes can also be matched:
--            @[:x:]@ within @[]@ specifies the class named @x@, which matches
--            certain predefined characters. See below for a full list.
--
-- [@[^..\]@ or @[!..\]@] Like @[..]@, but matches any character /not/ listed.
--                        Note that @[^-x]@ is not the inverse of @[-x]@, but
--                        the range @[^-x]@.
--
-- [@\<m-n>@] Matches any integer in the range m to n, inclusive. The range may
--            be open-ended by leaving out either number: @\"\<->\"@, for
--            instance, matches any integer.
--
-- [@**/@]    Matches any number of characters, including path separators,
--            excluding the empty string.
--
-- Supported character classes:
--
-- [@[:alnum:\]@]  Equivalent to @\"0-9A-Za-z\"@.
--
-- [@[:alpha:\]@]  Equivalent to @\"A-Za-z\"@.
--
-- [@[:blank:\]@]  Equivalent to @\"\\t \"@.
--
-- [@[:cntrl:\]@]  Equivalent to @\"\\0-\\x1f\\x7f\"@.
--
-- [@[:digit:\]@]  Equivalent to @\"0-9\"@.
--
-- [@[:graph:\]@]  Equivalent to @\"!-~\"@.
--
-- [@[:lower:\]@]  Equivalent to @\"a-z\"@.
--
-- [@[:print:\]@]  Equivalent to @\" -~\"@.
--
-- [@[:punct:\]@]  Equivalent to @\"!-\/:-\@[-`{-~\"@.
--
-- [@[:space:\]@]  Equivalent to @\"\\t-\\r \"@.
--
-- [@[:upper:\]@]  Equivalent to @\"A-Z\"@.
--
-- [@[:xdigit:\]@] Equivalent to @\"0-9A-Fa-f\"@.
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
-- Error recovery will be performed: erroneous operators will not be considered
-- operators, but matched as literal strings. Such operators include:
--
-- * An empty @[]@ or @[^]@ or @[!]@
--
-- * A @[@ or @\<@ without a matching @]@ or @>@
--
-- * A malformed @\<>@: e.g. nonnumeric characters or no hyphen
--
-- So, e.g. @[]@ will match the string @\"[]\"@.
compile :: String -> Pattern
compile = compileWith compDefault

-- |Like 'compile', but recognizes operators according to the given
-- 'CompOptions' instead of the defaults.
--
-- If an error occurs and 'errorRecovery' is disabled, 'error' will be called.
compileWith :: CompOptions -> String -> Pattern
compileWith opts = either error id . tryCompileWith opts

-- |A safe version of 'compileWith'.
--
-- If an error occurs and 'errorRecovery' is disabled, the error message will
-- be returned in a 'Left'.
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
   ors  = numberRanges       opts

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
charRange opts zs =
   case zs of
        y:ys | y `elem` "^!" ->
           case ys of
                -- [!-#] is not the inverse of [-#], it is the range ! through
                -- #
                '-':']':xs -> (Right (CharRange False [Left '-']), xs)
                '-'    :_  -> first (fmap (CharRange True )) (start zs)
                xs         -> first (fmap (CharRange False)) (start xs)
        _                  -> first (fmap (CharRange True )) (start zs)
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
   go (    ']':xs) = return xs
   go (      c:xs) =
      if not (pathSepInRanges opts) && isPathSeparator c
         then throwError "compile :: path separator within []"
         else char c xs
   go []           = throwError "compile :: unclosed [] in pattern"

   char :: Char -> String -> ErrorT String (Writer CharRange) String
   char c ('-':x:xs) =
      if x == ']'
         then ltell [Left c, Left '-'] >> return xs
         else ltell [Right (c,x)]      >>     go xs

   char c xs = ltell [Left c] >> go xs

   readClass :: String -> ErrorT String (Writer CharRange) String
   readClass xs = let (name,end) = span isAlpha xs
                   in case end of
                           ':':']':rest -> charClass name            >> go rest
                           _            -> ltell [Left '[',Left ':'] >> go xs

   charClass :: String -> ErrorT String (Writer CharRange) ()
   charClass name =
      -- The POSIX classes
      --
      -- TODO: this is ASCII-only, not sure how this should be extended
      --       Unicode, or with a locale as input, or something else?
      case name of
           "alnum"  -> ltell [digit,upper,lower]
           "alpha"  -> ltell [upper,lower]
           "blank"  -> ltell blanks
           "cntrl"  -> ltell [Right ('\0','\x1f'), Left '\x7f']
           "digit"  -> ltell [digit]
           "graph"  -> ltell [Right ('!','~')]
           "lower"  -> ltell [lower]
           "print"  -> ltell [Right (' ','~')]
           "punct"  -> ltell punct
           "space"  -> ltell spaces
           "upper"  -> ltell [upper]
           "xdigit" -> ltell [digit, Right ('A','F'), Right ('a','f')]
           _        ->
              throwError ("compile :: unknown character class '" ++name++ "'")

   digit  = Right ('0','9')
   upper  = Right ('A','Z')
   lower  = Right ('a','z')
   punct  = map Right [('!','/'), (':','@'), ('[','`'), ('{','~')]
   blanks = [Left '\t',         Left ' ']
   spaces = [Right ('\t','\r'), Left ' ']

   ltell = lift . tell


------------------------------------------
-- OPTIMIZATION
------------------------------------------


optimize :: Pattern -> Pattern
optimize = liftP (fin . go)
 where
   fin [] = []

   -- Literals to LongLiteral
   -- Has to be done here: we can't backtrack in go, but some cases might
   -- result in consecutive Literals being generated.
   -- E.g. "a[b]".
   fin (x:y:xs) | isLiteral x && isLiteral y =
      let (ls,rest) = span isLiteral xs
       in fin $ LongLiteral (length ls + 2)
                            (foldr (\(Literal a) -> (a:)) [] (x:y:ls))
                : rest

   -- concatenate LongLiterals
   -- Has to be done here because LongLiterals are generated above.
   --
   -- So one could say that we have one pass (go) which flattens everything as
   -- much as it can and one pass (fin) which concatenates what it can.
   fin (LongLiteral l1 s1 : LongLiteral l2 s2 : xs) =
      fin $ LongLiteral (l1+l2) (s1++s2) : xs

   fin (LongLiteral l s : Literal c : xs) =
      fin $ LongLiteral (l+1) (s++[c]) : xs

   fin (LongLiteral 1 s : xs) = Literal (head s) : fin xs

   fin (Literal c : LongLiteral l s : xs) =
      fin $ LongLiteral (l+1) (c:s) : xs

   fin (x:xs) = x : fin xs

   go [] = []
   go (x@(CharRange _ _) : xs) =
      case optimizeCharRange x of
           x'@(CharRange _ _) -> x' : go xs
           x'                 -> go (x':xs)

   -- <a-a> -> a
   go (OpenRange (Just a) (Just b):xs)
      | a == b = LongLiteral (length a) a : go xs

   -- <a-b> -> [a-b]
   -- a and b are guaranteed non-null
   go (OpenRange (Just [a]) (Just [b]):xs)
      | b > a = go $ CharRange True [Right (a,b)] : xs

   go (x:xs) =
      case find ($ x) compressors of
           Just c  -> let (compressed,ys) = span c xs
                       in if null compressed
                             then x : go ys
                             else go (x : ys)
           Nothing -> x : go xs

   compressors = [isStar, isStarSlash, isAnyNumber]

   isLiteral   (Literal _)                 = True
   isLiteral   _                           = False
   isStar      AnyNonPathSeparator         = True
   isStar      _                           = False
   isStarSlash AnyDirectory                = True
   isStarSlash _                           = False
   isAnyNumber (OpenRange Nothing Nothing) = True
   isAnyNumber _                           = False

optimizeCharRange :: Token -> Token
optimizeCharRange (CharRange b_ rs) = fin b_ . go . sortCharRange $ rs
 where
   -- [/] is interesting, it actually matches nothing at all
   -- [.] can be Literalized though, just don't make it into an ExtSeparator so
   --     that it doesn't match a leading dot
   fin True [Left  c] | not (isPathSeparator c) = Literal c
   fin True [Right r] | r == (minBound,maxBound) = NonPathSeparator
   fin b x = CharRange b x

   go [] = []

   go (x@(Left c) : xs) =
      case xs of
           [] -> [x]
           y@(Left d) : ys
              -- [aaaaa] -> [a]
              | c == d      -> go$ Left c : ys
              | d == succ c ->
                 let (ls,rest)        = span isLeft xs -- start from y
                     (catable,others) = increasingSeq (map fromLeft ls)
                     range            = (c, head catable)

                  in -- three (or more) Lefts make a Right
                     if null catable || null (tail catable)
                        then x : y : go ys
                        -- [abcd] -> [a-d]
                        else go$ Right range : map Left others ++ rest

              | otherwise -> x : go xs

           Right r : ys ->
              case addToRange r c of
                   -- [da-c] -> [a-d]
                   Just r' -> go$ Right r' : ys
                   Nothing -> x : go xs

   go (x@(Right r) : xs) =
      case xs of
           [] -> [x]
           Left c : ys ->
              case addToRange r c of
                   -- [a-cd] -> [a-d]
                   Just r' -> go$ Right r' : ys
                   Nothing -> x : go xs

           Right r' : ys ->
              case overlap r r' of
                   -- [a-cb-d] -> [a-d]
                   Just o  -> go$ Right o : ys
                   Nothing -> x : go xs
optimizeCharRange _ = error "Glob.optimizeCharRange :: internal error"

sortCharRange :: [Either Char (Char,Char)] -> [Either Char (Char,Char)]
sortCharRange = sortBy cmp
 where
   cmp (Left   a)    (Left   b)    = compare a b
   cmp (Left   a)    (Right (b,_)) = compare a b
   cmp (Right (a,_)) (Left   b)    = compare a b
   cmp (Right (a,_)) (Right (b,_)) = compare a b
