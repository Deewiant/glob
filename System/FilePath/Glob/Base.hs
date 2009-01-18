-- File created: 2008-10-10 13:29:26

module System.FilePath.Glob.Base where

import Control.Arrow     (first)
import Control.Exception (assert)
import Data.Maybe        (fromMaybe, isJust)
import Data.Monoid       (Monoid, mappend, mempty)
import System.FilePath   ( pathSeparator, extSeparator
                         , isExtSeparator, isPathSeparator
                         )

-- |These are the compilation options.  We could presumably put
-- locale information in here, too.  (note that some of these options
-- depend on each other: classes can never occur if ranges aren't allowed)
data CompOptions = CompOptions {
      characterClasses   :: Bool -- ^allow character classes @[[:...:]]@
    , characterRanges    :: Bool -- ^allow character ranges @[...]@
    , openRanges         :: Bool -- ^allow open ranges @<...>@
    , recursiveWildcards :: Bool -- ^allow recursive wildcards @**/@
    , wildcards          :: Bool -- ^allow wildcards at all (@*@ and @?@)
    , errorRecovery      :: Bool -- ^make special chars literal to recover
    }

compExtended :: CompOptions
compExtended = CompOptions { characterClasses = False
                           , characterRanges  = True
                           , openRanges = True
                           , recursiveWildcards = True
                           , wildcards = True
                           , errorRecovery = False }

compPosix :: CompOptions
compPosix = CompOptions { characterClasses = True
                        , characterRanges = True
                        , openRanges = False
                        , recursiveWildcards = False
                        , wildcards = True
                        , errorRecovery = True }

-- |These are the options that matter at match-time.
data ExecOptions = ExecOptions {
      matchDots   :: Bool -- ^allow wildcards to match @.@ at start of names
    , caseless    :: Bool -- ^case-independent matching (i.e. tolower)
    }

defaultExecOpts :: ExecOptions
defaultExecOpts = ExecOptions { matchDots = False
                              , caseless  = False }

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

-- |An abstract data type representing a compiled pattern.
-- 
-- The 'Show' instance is essentially the inverse of @'compile'@. Though it may
-- not return exactly what was given to @'compile'@ it will return code which
-- produces the same 'Pattern'.
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
   show (LongLiteral _ s)   = s
   show (CharRange b r)     =
      '[' : (if b then "" else "^") ++
            concatMap (either (:[]) (\(x,y) -> [x,'-',y])) r ++ "]"
   show (OpenRange a b)     =
      '<' : fromMaybe "" a ++ "-" ++
            fromMaybe "" b ++ ">"

   showList = showList . concatMap show

instance Show Pattern where
   showsPrec d (Pattern ts) =
      showParen (d > 10) $ showString "compile " . shows ts

-- it might be better to write mconcat instead?  and then just use
-- optimize somehow?  (this is problemmatic because we'd probably
-- end up with a floating instance)
-- Shouldn't `mappend` be infixr?
instance Monoid Pattern where
   mempty = Pattern []

   mappend (Pattern []) b = b
   mappend (Pattern a) (Pattern (b:bs)) | isJust (fromLiteral b) =
      let Just b' = fromLiteral b
          (a',l) = splitLast a
       in case fromLiteral l of
               Just l' -> Pattern $ a'++(longLiteral $ l'++b'):bs
               _       -> Pattern (a++(b:bs))

    where
      splitLast [x]    = ([],x)
      splitLast (x:xs) = first (x:) (splitLast xs)
      splitLast _      =
         error "System.FilePath.Glob.Pattern.mappend :: internal error"

      fromLiteral (Literal c)       = Just [c]
      fromLiteral (LongLiteral _ s) = Just s
      fromLiteral _                 = Nothing

      longLiteral s = LongLiteral (length s) s

   mappend (Pattern a) (Pattern b) = Pattern $ a++b
