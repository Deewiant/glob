-- File created: 2008-10-11 11:11:30

module Compiler
   ( prop_decompileCompile
   ) where

import System.FilePath.Glob.Compile

import Base

prop_decompileCompile s =
   let
      (_:(a,ad):(b,bd):_) =
         iterate (\(_,a) -> let x = compile a
                             in (x, decompile (fromRight x)))
                 (undefined,unPS s)
    in isRight a && isRight b && ad == bd
