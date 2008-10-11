-- File created: 2008-10-11 11:11:30

module Compiler
   ( prop_decompileTokenize
   , prop_decompileCompile
   ) where

import System.FilePath.Glob.Compile

import Base

prop_decompileTokenize s =
   let a  = tokenize (unPS s)
       ad = decompile . fromRight $ a
       b  = tokenize ad
       bd = decompile . fromRight $ b
    in isRight a && isRight b && ad == bd

prop_decompileCompile s =
   let a  = compile (unPS s)
       ad = decompile . fromRight $ a
       b  = compile ad
       bd = decompile . fromRight $ b
    in isRight a && isRight b && ad == bd
