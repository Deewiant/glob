-- File created: 2008-10-10 16:23:56

module Main (main) where

import System.Environment (getArgs)
import Test.Framework

import qualified Tests.Compiler   as Compiler
import qualified Tests.Instances  as Instances
import qualified Tests.Matcher    as Matcher
import qualified Tests.Optimizer  as Optimizer
import qualified Tests.Regression as Regression
import qualified Tests.Simplifier as Simplifier
import qualified Tests.Utils      as Utils

main = do
   args <- getArgs
   defaultMainWithArgs tests . concat $
      [ ["--timeout", show 10]
      , ["--maximum-generated-tests", show 1000]
      , args
      ]

tests =
   [ Regression.tests
   , Utils.tests
   , Compiler.tests
   , Matcher.tests
   , Optimizer.tests
   , Simplifier.tests
   , Instances.tests
   ]
