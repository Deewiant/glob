-- File created: 2008-10-16 12:12:50

module System.FilePath.Glob.Directory
   ( GlobOptions(..), globDefault
   , globDir, globDirWith, globDir1, glob
   , commonDirectory
   ) where

import Control.Arrow    (first, second)
import Control.Monad    (forM)
import qualified Data.DList as DL
import Data.DList       (DList)
import Data.List        ((\\), stripPrefix, find)
import System.Directory ( doesDirectoryExist, getDirectoryContents
                        , getCurrentDirectory
                        )
import System.FilePath  ( (</>), takeDrive, splitDrive
                        , extSeparator, isExtSeparator
                        , pathSeparator, isPathSeparator
                        , takeDirectory
                        )

import System.FilePath.Glob.Base  ( Pattern(..), Token(..)
                                  , MatchOptions, matchDefault
                                  , compile
                                  )
import System.FilePath.Glob.Match (matchWith)
import System.FilePath.Glob.Utils ( getRecursiveContents
                                  , nubOrd
                                  , pathParts
                                  , partitionDL
                                  , catchIO
                                  )
-- |Options which can be passed to the 'globDirWith' function.
data GlobOptions = GlobOptions
  { matchOptions :: MatchOptions
  -- ^Options controlling how matching is performed; see 'MatchOptions'.
  , includeUnmatched :: Bool
  -- ^Whether to include unmatched files in the result.
  }

-- |The default set of globbing options: uses the default matching options, and
-- does not include unmatched files.
globDefault :: GlobOptions
globDefault = GlobOptions matchDefault False

-- The Patterns in TypedPattern don't contain PathSeparator or AnyDirectory
--
-- We store the number of PathSeparators that Dir and AnyDir were followed by
-- so that "foo////*" can match "foo/bar" but return "foo////bar". It's the
-- exact number for convenience: (</>) doesn't add a path separator if one is
-- already there. This way, '\(Dir n _) -> replicate n pathSeparator </> "bar"'
-- results in the correct amount of slashes.
data TypedPattern
   = Any Pattern        -- pattern
   | Dir Int Pattern    -- pattern/
   | AnyDir Int Pattern -- pattern**/
   deriving Show

-- |Matches each given 'Pattern' against the contents of the given 'FilePath',
-- recursively. The result contains the matched paths, grouped for each given
-- 'Pattern'. The results are not in any defined order.
--
-- The given directory is prepended to all the matches: the returned paths are
-- all valid from the point of view of the current working directory.
--
-- If multiple 'Pattern's match a single 'FilePath', that path will be included
-- in multiple groups.
--
-- Two 'FilePath's which can be canonicalized to the same file (e.g. @\"foo\"@
-- and @\"./foo\"@) may appear separately if explicit matching on paths
-- beginning with @\".\"@ is done. Looking for @\".*/*\"@, for instance, will
-- cause @\"./foo\"@ to return as a match but @\"foo\"@ to not be matched.
--
-- This function is different from a simple 'filter' over all the contents of
-- the directory: the matching is performed relative to the directory, so that
-- for instance the following is true:
--
-- > fmap head (globDir [compile "*"] dir) == getDirectoryContents dir
--
-- (With the exception that that glob won't match anything beginning with @.@.)
--
-- If the given 'FilePath' is @[]@, 'getCurrentDirectory' will be used.
--
-- If the given 'Pattern' starts with a drive (as defined by
-- 'System.FilePath'), it is not relative to the given directory and the
-- 'FilePath' parameter is completely ignored! Similarly, if the given
-- 'Pattern' starts with a path separator, only the drive part of the
-- 'FilePath' is used. On Posix systems these behaviours are equivalent:
-- 'Pattern's starting with @\/@ work relative to @\/@. On Windows, 'Pattern's
-- starting with @\/@ or @\\@ work relative only to the drive part of the
-- 'FilePath' and 'Pattern's starting with absolute paths ignore the
-- 'FilePath'.
--
-- Note that in some cases results outside the given directory may be returned:
-- for instance the @.*@ pattern matches the @..@ directory.
--
-- Any results deeper than in the given directory are enumerated lazily, using
-- 'unsafeInterleaveIO'.
--
-- Directories without read permissions are returned as entries but their
-- contents, of course, are not.
globDir :: [Pattern] -> FilePath -> IO [[FilePath]]
globDir pats dir = fmap fst $ globDirWith globDefault pats dir

-- |Like 'globDir', but applies the given 'GlobOptions' instead of the
-- defaults when matching. The first component of the returned tuple contains
-- the matched paths, grouped for each given 'Pattern', and the second contains
-- Just the unmatched paths if the given 'GlobOptions' specified that unmatched
-- files should be included, or otherwise Nothing.
globDirWith :: GlobOptions -> [Pattern] -> FilePath
            -> IO ([[FilePath]], Maybe [FilePath])
globDirWith opts [pat] dir | not (includeUnmatched opts) =
   -- This is an optimization for the case where only one pattern has been
   -- passed and we are not including unmatched files: we can use
   -- 'commonDirectory' to avoid some calls to 'getDirectoryContents'.
   let (prefix, pat') = commonDirectory pat
    in globDirWith' opts [pat'] (dir </> prefix)

globDirWith opts pats dir =
   globDirWith' opts pats dir

-- See 'globDirWith'.
globDirWith' :: GlobOptions -> [Pattern] -> FilePath
            -> IO ([[FilePath]], Maybe [FilePath])
globDirWith' opts []   dir =
   if includeUnmatched opts
      then do
         dir' <- if null dir then getCurrentDirectory else return dir
         c <- getRecursiveContents dir'
         return ([], Just (DL.toList c))
      else
         return ([], Nothing)

globDirWith' opts pats@(_:_) dir = do
   results <- mapM (\p -> globDir'0 opts p dir) pats

   let (matches, others) = unzip results
       allMatches        = DL.toList . DL.concat $ matches
       allOthers         = DL.toList . DL.concat $ others

   return ( map DL.toList matches
          , if includeUnmatched opts
               then Just (nubOrd allOthers \\ allMatches)
               else Nothing
          )

-- |A convenience wrapper on top of 'globDir', for when you only have one
-- 'Pattern' you care about. Returns only the matched paths.
globDir1 :: Pattern -> FilePath -> IO [FilePath]
globDir1 p = fmap head . globDir [p]

-- |The simplest IO function. Finds matches to the given pattern in the current
-- working directory. Takes a 'String' instead of a 'Pattern' to avoid the need
-- for a call to 'compile', simplifying usage further.
--
-- Can also be seen as a convenience wrapper on top of 'globDir1', for when you
-- want to work in the current directory or have a pattern referring to an
-- absolute path.
glob :: String -> IO [FilePath]
glob = flip globDir1 "" . compile

globDir'0 :: GlobOptions -> Pattern -> FilePath
          -> IO (DList FilePath, DList FilePath)
globDir'0 opts pat dir = do
   let (pat', drive) = driveSplit pat
   dir' <- case drive of
                Just "" -> fmap takeDrive getCurrentDirectory
                Just d  -> return d
                Nothing -> if null dir then getCurrentDirectory else return dir
   globDir' opts (separate pat') dir'

globDir' :: GlobOptions -> [TypedPattern] -> FilePath
         -> IO (DList FilePath, DList FilePath)
globDir' opts pats@(_:_) dir = do
   entries <- getDirectoryContents dir `catchIO` const (return [])

   results <- forM entries $ \e -> matchTypedAndGo opts pats e (dir </> e)

   let (matches, others) = unzip results

   return (DL.concat matches, DL.concat others)

globDir' _ [] dir =
   -- We can only get here from matchTypedAndGo getting a [Dir _]: it means the
   -- original pattern had a trailing PathSeparator. Reproduce it here.
   return (DL.singleton (dir ++ [pathSeparator]), DL.empty)

matchTypedAndGo :: GlobOptions
                -> [TypedPattern]
                -> FilePath -> FilePath
                -> IO (DList FilePath, DList FilePath)

-- (Any p) is always the last element
matchTypedAndGo opts [Any p] path absPath =
   if matchWith (matchOptions opts) p path
      then return (DL.singleton absPath, DL.empty)
      else doesDirectoryExist absPath >>= didn'tMatch opts path absPath

matchTypedAndGo opts (Dir n p:ps) path absPath = do
   isDir <- doesDirectoryExist absPath
   if isDir && matchWith (matchOptions opts) p path
      then globDir' opts ps (absPath ++ replicate n pathSeparator)
      else didn'tMatch opts path absPath isDir

matchTypedAndGo opts (AnyDir n p:ps) path absPath = do
   if path `elem` [".",".."]
      then didn'tMatch opts path absPath True
      else do
         isDir <- doesDirectoryExist absPath
         let m = matchWith (matchOptions opts) (unseparate ps)
             unconditionalMatch =
                null (unPattern p) && not (isExtSeparator $ head path)
             p' = Pattern (unPattern p ++ [AnyNonPathSeparator])

         case unconditionalMatch || matchWith (matchOptions opts) p' path of
              True | isDir -> do
                 contents <- getRecursiveContents absPath
                 return $
                    -- foo**/ should match foo/ and nothing below it
                    -- relies on head contents == absPath
                    if null ps
                       then ( DL.singleton $
                                DL.head contents
                                ++ replicate n pathSeparator
                            , DL.tail contents
                            )
                       else let (matches, nonMatches) =
                                   partitionDL fst
                                      (fmap (recursiveMatch n m) contents)
                             in (fmap snd matches, fmap snd nonMatches)

              True | m path ->
                 return ( DL.singleton $
                             takeDirectory absPath
                             ++ replicate n pathSeparator
                             ++ path
                        , DL.empty
                        )
              _ ->
                 didn'tMatch opts path absPath isDir

matchTypedAndGo _ _ _ _ = error "Glob.matchTypedAndGo :: internal error"

recursiveMatch :: Int -> (FilePath -> Bool) -> FilePath -> (Bool, FilePath)
recursiveMatch n isMatch path =
   case find isMatch (pathParts path) of
        Just matchedSuffix ->
           case stripSuffix matchedSuffix path of
                Just dir ->
                   ( True
                   , dir
                     ++ replicate (n-1) pathSeparator
                     ++ matchedSuffix
                   )
                Nothing ->
                   -- the fact that we won't hit this branch relies on the
                   -- property that every element of `pathParts fp` is a suffix
                   -- of `fp`.
                   error "Glob.recursiveMatch :: internal error"
        Nothing ->
           (False, path)

   where
   stripSuffix suffix =
      fmap reverse . stripPrefix (reverse suffix) . reverse

-- To be called when a pattern didn't match a path: given the path and whether
-- it was a directory, return all paths which didn't match (i.e. for a file,
-- just the file, and for a directory, everything inside it).
didn'tMatch :: GlobOptions -> FilePath -> FilePath -> Bool
            -> IO (DList FilePath, DList FilePath)
didn'tMatch opts path absPath isDir =
   if includeUnmatched opts
      then (fmap $ (,) DL.empty) $
         if isDir
            then if path `elem` [".",".."]
                    then return DL.empty
                    else getRecursiveContents absPath
            else return$ DL.singleton absPath
      else
         return (DL.empty, DL.empty)

separate :: Pattern -> [TypedPattern]
separate = go DL.empty . unPattern
 where
   go gr [] | null (DL.toList gr) = []
   go gr []                       = [Any (pat gr)]
   go gr (PathSeparator:ps)       = slash gr Dir ps
   go gr ( AnyDirectory:ps)       = slash gr AnyDir ps
   go gr (            p:ps)       = go (gr `DL.snoc` p) ps

   pat = Pattern . DL.toList

   slash gr f ps = let (n,ps') = first length . span isSlash $ ps
                    in f (n+1) (pat gr) : go DL.empty ps'

   isSlash PathSeparator = True
   isSlash _             = False

unseparate :: [TypedPattern] -> Pattern
unseparate = Pattern . foldr f []
 where
   f (AnyDir n p) ts = u p ++ AnyDirectory : replicate (n-1) PathSeparator ++ ts
   f (   Dir n p) ts = u p ++ replicate n PathSeparator ++ ts
   f (Any      p) ts = u p ++ ts

   u = unPattern

-- Note that we consider "/foo" to specify a drive on Windows, even though it's
-- relative to the current drive.
--
-- Returns the [TypedPattern] of the Pattern (with the drive dropped if
-- appropriate) and, if the Pattern specified a drive, a Maybe representing the
-- drive to use. If it's a Just "", use the drive of the current working
-- directory.
driveSplit :: Pattern -> (Pattern, Maybe FilePath)
driveSplit = check . split . unPattern
 where
   -- We can't just use something like commonDirectory because of Windows
   -- drives being possibly longer than one "directory", like "//?/foo/bar/".
   -- So just take as much as possible.
   split (LongLiteral _ l : xs) = first (l++) (split xs)
   split (    Literal   l : xs) = first (l:) (split xs)
   split (PathSeparator   : xs) = first (pathSeparator:) (split xs)
   split ( ExtSeparator   : xs) = first ( extSeparator:) (split xs)
   split xs                     = ([],xs)

   -- The isPathSeparator check is interesting in two ways:
   --
   -- 1. It's correct to return simply Just "" because there can't be more than
   --    one path separator if splitDrive gave a null drive: "//x" is a shared
   --    "drive" in Windows and starts with the root "drive" in Posix.
   --
   -- 2. The 'head' is safe because we have not (null d) && null drive.
   check (d,ps)
      | null d                      = (Pattern     ps, Nothing)
      | not (null drive)            = (dirify rest ps, Just drive)
      | isPathSeparator (head rest) = (Pattern     ps, Just "")
      | otherwise                   = (dirify d    ps, Nothing)
    where
      (drive, rest) = splitDrive d

   dirify path = Pattern . (comp path++)

   comp s = let (p,l) = foldr f ([],[]) s in if null l then p else ll l p
    where
      f c (p,l) | isExtSeparator  c = (ExtSeparator  : ll l p, [])
                | isPathSeparator c = (PathSeparator : ll l p, [])
                | otherwise         = (p, c:l)

      ll l p = if null l then p else LongLiteral (length l) l : p

-- |Factors out the directory component of a 'Pattern'. Useful in conjunction
-- with 'globDir'.
--
-- Preserves the number of path separators: @commonDirectory (compile
-- \"foo\/\/\/bar\")@ becomes @(\"foo\/\/\/\", compile \"bar\")@.
commonDirectory :: Pattern -> (FilePath, Pattern)
commonDirectory = second unseparate . splitP . separate
 where
   splitP pt@(Dir n p:ps) | not (startsWithImplicitExt p) =
      case fromConst DL.empty (unPattern p) of
           Just d  -> first ((d ++ replicate n pathSeparator) </>) (splitP ps)
           Nothing -> ("", pt)

   splitP pt = ("", pt)

   fromConst d []                   = Just (DL.toList d)
   fromConst d (Literal c      :xs) = fromConst (d `DL.snoc` c) xs
   fromConst d (ExtSeparator   :xs) = fromConst (d `DL.snoc` extSeparator) xs
   fromConst d (LongLiteral _ s:xs) = fromConst (d `DL.append`DL.fromList s) xs
   fromConst _ _                    = Nothing

   -- "." must be explicitly matched at the start of a pattern or after a path
   -- separator; this function checks for an implicit match i.e. "[.]".
   startsWithImplicitExt p =
      case unPattern p of
         (Literal '.' : _) -> True
         (LongLiteral _ ('.':_) : _) -> True
         _ -> False
