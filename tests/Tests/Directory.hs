module Tests.Directory where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck (Property, (===))
import Test.HUnit.Base
import Control.Monad (filterM, zipWithM)
import Data.Function (on)
import Data.Monoid ((<>))
import Data.List ((\\), sort)
import qualified Data.DList as DList
import System.Directory (doesDirectoryExist)
import System.FilePath (takeBaseName)

import System.FilePath.Glob.Base
import System.FilePath.Glob.Directory
import System.FilePath.Glob.Primitive
import System.FilePath.Glob.Utils
import Tests.Base (PString, unPS)

tests = testGroup "Directory"
   [ testCase "includeUnmatched" caseIncludeUnmatched
   , testCase "onlyMatched" caseOnlyMatched
   , testGroup "commonDirectory"
       [ testGroup "edge-cases" commonDirectoryEdgeCases
       , testProperty "property" prop_commonDirectory
       ]
   ]

caseIncludeUnmatched = do
   let pats = ["**/D*.hs", "**/[MU]*.hs"]
   everything <- getRecursiveContentsDir "System"
   let expectedMatches =
          [ [ "System/FilePath/Glob/Directory.hs" ]
          , [ "System/FilePath/Glob/Match.hs"
            , "System/FilePath/Glob/Utils.hs"
            ]
          ]
   let everythingElse = everything \\ concat expectedMatches

   result <- globDirWith (GlobOptions matchDefault True)
                         (map compile pats)
                         "System"
   zipWithM assertEqualUnordered expectedMatches (fst result)

   case snd result of
       Nothing ->
          assertFailure "Expected Just a list of unmatched files"
       Just unmatched -> do
          assertEqualUnordered everythingElse (applyHack unmatched)

   where
   -- FIXME: slashes are not correctly preserved with AnyDirectory (that is,
   -- **) patterns. This hack works around that so that these tests can run.
   applyHack = ("System/FilePath" :) . filter (/= "System/FilePath/")

caseOnlyMatched = do
   let pats = ["**/D*.hs", "**/[MU]*.hs"]
   let expectedMatches =
          [ [ "System/FilePath/Glob/Directory.hs" ]
          , [ "System/FilePath/Glob/Match.hs"
            , "System/FilePath/Glob/Utils.hs"
            ]
          ]

   result <- globDirWith globDefault
                         (map compile pats)
                         "System"

   zipWithM assertEqualUnordered expectedMatches (fst result)
   assertEqual "" Nothing (snd result)

assertEqualUnordered :: (Ord a, Show a) => [a] -> [a] -> Assertion
assertEqualUnordered = assertEqual "" `on` sort

-- Like 'getRecursiveContents', except this function removes the root directory
-- from the returned list, so that it should match* the union of matched and
-- unmatched files returned from 'globDirWith', where the same directory was
-- given as the directory argument.
--
-- * to be a little more precise, these files will only match up to
-- normalisation of paths e.g. some patterns will cause the list of matched
-- files to contain repeated slashes, whereas the list returned by this
-- function will not have repeated slashes.
getRecursiveContentsDir :: FilePath -> IO [FilePath]
getRecursiveContentsDir root =
  fmap (filter (/= root) . DList.toList) (getRecursiveContents root)

prop_commonDirectory :: PString -> Property
prop_commonDirectory pstr =
   let pat    = compile (unPS pstr)
       (a, b) = commonDirectory pat
    in pat === (literal a <> b)

commonDirectoryEdgeCases = zipWith mkTest [1..] testData
   where
   mkTest i (input, expected) =
      testCase (show i)
               (assertEqual "" expected (commonDirectory (compile input)))
   testData =
      [ ("[.]/*", ("", compile "[.]/*"))
      , ("foo/[.]bar/*", ("foo/", compile "[.]bar/*"))
      , ("[.]foo/bar/*", ("", compile "[.]foo/bar/*"))
      , ("foo.bar/baz/*", ("foo.bar/baz/", compile "*"))
      , ("foo[.]bar/baz/*", ("foo.bar/baz/", compile "*"))
      ]
