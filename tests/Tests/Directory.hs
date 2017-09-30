module Tests.Directory where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck (Property, (===))
import Test.HUnit.Base hiding (Test)
import Data.Function (on)
import Data.Monoid ((<>))
import Data.List ((\\), sort)
import qualified Data.DList as DList

import System.FilePath.Glob.Base
import System.FilePath.Glob.Directory
import System.FilePath.Glob.Primitive
import System.FilePath.Glob.Utils
import Tests.Base (PString, unPS)

tests :: Test
tests = testGroup "Directory"
   [ testCase "includeUnmatched" caseIncludeUnmatched
   , testCase "onlyMatched" caseOnlyMatched
   , testGroup "commonDirectory"
       [ testGroup "edge-cases" testsCommonDirectoryEdgeCases
       , testProperty "property" prop_commonDirectory
       ]
   , testCase "globDir1" caseGlobDir1
   , testGroup "repeated-path-separators" testsRepeatedPathSeparators
   ]

caseIncludeUnmatched :: Assertion
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
   mapM_ (uncurry assertEqualUnordered) (zip expectedMatches (fst result))

   case snd result of
       Nothing ->
          assertFailure "Expected Just a list of unmatched files"
       Just unmatched -> do
          assertEqualUnordered everythingElse unmatched

caseOnlyMatched :: Assertion
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

   mapM_ (uncurry assertEqualUnordered) (zip expectedMatches (fst result))
   assertEqual "" Nothing (snd result)

caseGlobDir1 :: Assertion
caseGlobDir1 = do
   -- this is little a bit of a hack; we pass the same pattern twice to ensure
   -- that the optimization in the single pattern case is bypassed
   let naiveGlobDir1 p = fmap head . globDir [p, p]
   let pat = compile "FilePath/*/*.hs"
   let dir = "System"

   actual <- globDir1 pat dir
   expected <- naiveGlobDir1 pat dir
   assertEqual "" expected actual

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

-- These two patterns should always be equal
prop_commonDirectory' :: String -> (Pattern, Pattern)
prop_commonDirectory' str =
   let pat    = compile str
       (a, b) = commonDirectory pat
    in (pat, literal a <> b)

prop_commonDirectory :: PString -> Property
prop_commonDirectory = uncurry (===) . prop_commonDirectory' . unPS

testsCommonDirectoryEdgeCases :: [Test]
testsCommonDirectoryEdgeCases = zipWith mkTest [1 :: Int ..] testData
 where
   mkTest i (input, expected) =
      testCase (show i) $ do
         assertEqual "" expected (commonDirectory (compile input))
         uncurry (assertEqual "") (prop_commonDirectory' input)

   testData =
      [ ("[.]/*", ("", compile "[.]"))
      , ("foo/[.]bar/*", ("", compile "[.]"))
      , ("[.]foo/bar/*", ("", compile "[.]foo/bar/*"))
      , ("foo.bar/baz/*", ("foo.bar/baz/", compile "*"))
      , ("[f]oo[.]/bar/*", ("foo./bar/", compile "*"))
      , ("foo[.]bar/baz/*", ("foo.bar/baz/", compile "*"))
      , (".[.]/foo/*", ("../foo/", compile "*"))
      ]

-- see #16
testsRepeatedPathSeparators :: [Test]
testsRepeatedPathSeparators = zipWith mkTest [1 :: Int ..] testData
 where
   mkTest i (dir, pat, expected) =
      testCase (show i) $ do
         actual <- globDir1 (compile pat) dir
         assertEqualUnordered expected actual

   testData =
      [ ( "System"
        , "*//Glob///[U]*.hs"
        , [ "System/FilePath//Glob///Utils.hs"
          ]
        )
      , ( "System"
        , "**//[GU]*.hs"
        , [ "System/FilePath//Glob.hs"
          , "System/FilePath/Glob//Utils.hs"
          ]
        )
      , ( "System"
        , "File**/"
        , [ "System/FilePath/"
          ]
        )
      , ( "System"
        , "File**//"
        , [ "System/FilePath//"
          ]
        )
      , ( "System"
        , "File**///"
        , [ "System/FilePath///"
          ]
        )
      , ( "System/FilePath"
        , "**//Glob.hs"
        , [ "System/FilePath//Glob.hs"
          ]
        )
      , ( "System"
        , "**Path/Glob//Utils.hs"
        , [ "System/FilePath/Glob//Utils.hs"
          ]
        )
      ]
