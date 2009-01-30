-- File created: 2009-01-30 13:26:51

module Tests.Compiler (tests) where

import Test.Framework
import Test.Framework.Providers.QuickCheck

import System.FilePath.Glob.Base (tryCompileWith, compile, decompile)

import Tests.Base

tests = testGroup "Compiler"
   [ testProperty "compile-decompile-1" prop_compileDecompile1
   ]

-- compile . decompile should be the identity function
prop_compileDecompile1 o s =
   let opt   = unCOpts o
       epat1 = tryCompileWith opt (unPS s)
       pat1  = fromRight epat1
       pat2  = compile . decompile $ pat1
    in isRight epat1 && pat1 == pat2
