-- File created: 2009-01-30 13:26:51

module Tests.Compiler (tests) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck (Property, (==>))

import System.FilePath.Glob.Base
   (CompOptions(..), compDefault, compile, decompile, isLiteral, tryCompileWith)

import Tests.Base

tests :: Test
tests = testGroup "Compiler"
   [ testProperty "compile-decompile-1" prop_compileDecompile1
   , testProperty "isliteral" prop_isLiteral
   ]

-- compile . decompile should be the identity function
prop_compileDecompile1 :: COpts -> PString -> Property
prop_compileDecompile1 o s =
   let opt   = unCOpts o
       epat1 = tryCompileWith opt (unPS s)
       pat1  = fromRight epat1
       pat2  = compile . decompile $ pat1
    in isRight epat1 ==> pat1 == pat2

prop_isLiteral :: PString -> Property
prop_isLiteral p =
   let epat = tryCompileWith noWildcardOptions (unPS p)
       pat = fromRight epat
    in isRight epat ==> (isLiteral . compile . decompile) pat
 where
   noWildcardOptions = compDefault
      { characterClasses   = False
      , characterRanges    = False
      , numberRanges       = False
      , wildcards          = False
      , recursiveWildcards = False
      }
