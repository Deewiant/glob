-- File created: 2008-10-10 13:40:35

module System.FilePath.Glob.Utils where

import Control.Exception (assert)

inRange :: Ord a => (a,a) -> a -> Bool
inRange (a,b) c = assert (b >= a) $ c >= a && c <= b
