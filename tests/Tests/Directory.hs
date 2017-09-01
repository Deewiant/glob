module Tests.Directory where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit.Base
import Control.Monad (filterM, zipWithM)
import Data.Function (on)
import Data.List ((\\), sort)
import qualified Data.DList as DList
import System.Directory (doesDirectoryExist)
import System.FilePath (takeBaseName)

import System.FilePath.Glob.Base
import System.FilePath.Glob.Directory
import System.FilePath.Glob.Utils

tests = testGroup "Directory"
   [ testCase "includeUnmatched" caseIncludeUnmatched
   , testCase "onlyMatched" caseOnlyMatched
   ]

caseIncludeUnmatched = do
   let pats = ["**/D*.hs", "**/[MU]*.hs"]
   everything <- (DList.toList <$> getRecursiveContents "System")
                 >>= removeDirectories
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
          noDirs <- removeDirectories unmatched
          assertEqualUnordered everythingElse noDirs

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

removeDirectories :: [FilePath] -> IO [FilePath]
removeDirectories = filterM (fmap not . doesDirectoryExist)
