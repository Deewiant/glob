-- File created: 2008-10-10 16:23:56

module Main (main) where

import System.Environment (getArgs)
import Test.Framework

import qualified Optimizer
import qualified Regression
import qualified Utils

main = do
   args <- getArgs
   defaultMainWithArgs tests . concat $
      [ ["--timeout", show 10]
      , ["--maximum-generated-tests", show 1000]
      , args
      ]

tests = concat
   [ Regression.tests
   , Optimizer.tests
   , Utils.tests
   ]
