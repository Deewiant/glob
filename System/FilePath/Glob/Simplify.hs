-- File created: 2009-01-30 14:54:14

module System.FilePath.Glob.Simplify (simplify) where

import System.FilePath.Glob.Base (Pattern(..), Token(..), liftP)

-- |Simplifies a 'Pattern' object: removes redundant @\"./\"@, for instance.
-- The resulting 'Pattern' matches the exact same input as the original one,
-- with some differences:
--
-- * The output of 'globDir' will differ: for example, globbing for @\"./\*\"@
--   gives @\"./foo\"@, but after simplification this'll be only @\"foo\"@.
--
-- * Decompiling the simplified 'Pattern' will obviously not give the original.
--
-- * The simplified 'Pattern' is a bit faster to match with and uses less
--   memory, since some redundant data is removed.
--
-- For the last of the above reasons, if you're performance-conscious and not
-- using 'globDir', you should always 'simplify' after calling 'compile'.
simplify :: Pattern -> Pattern
simplify = liftP (go . pre)
 where
   -- ./ at beginning -> nothing (any number of /'s)
   pre (ExtSeparator:PathSeparator:xs) = pre (dropWhile isSlash xs)
   pre                             xs  = xs

   go [] = []

   -- /./ -> /
   go (PathSeparator:ExtSeparator:xs@(PathSeparator:_)) = go xs

   go (x:xs) =
      if isSlash x
         then let (compressed,ys) = span isSlash xs
               in if null compressed
                     then x : go ys
                     else go (x : ys)
         else x : go xs

   isSlash PathSeparator = True
   isSlash _             = False
