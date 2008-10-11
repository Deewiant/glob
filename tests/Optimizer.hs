-- File created: 2008-10-11 11:18:31

module Optimizer
   ( prop_optimize1
   , prop_optimize2
   ) where

import System.FilePath.Glob.Compile
import System.FilePath.Glob.Optimize
import System.FilePath.Glob.Match

import Base

prop_optimize1 s =
   let pat = tokenize (unPS s)
       xs = iterate optimize (fromRight pat)
    in isRight pat && decompile (xs !! 1) == decompile (xs !! 2)

prop_optimize2 p s =
   let x   = tokenize (unPS p)
       pat = fromRight x
       pth = unP s
    in isRight x && match pat pth == match (optimize pat) pth
