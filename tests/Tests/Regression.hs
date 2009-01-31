-- File created: 2008-10-15 20:21:41

module Tests.Regression (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit.Base

import System.FilePath.Glob.Base
import System.FilePath.Glob.Match

tests = testGroup "Regression"
   [ testGroup "Matching/compiling" .
        flip map matchCases $ \t@(b,p,s) ->
            tc (nameMatchTest t) $
               match (compile p) s == b
   , testGroup "Specific options" .
        flip map matchWithCases $ \t@(b,co,mo,p,s) ->
           tc (nameMatchTest (b,p,s)) $
              matchWith mo (compileWith co p) s == b
   , testGroup "Decompilation" .
        flip map decompileCases $ \(n,orig,s) ->
           tc n $ decompile (compile orig) == s
   ]
 where
   tc n = testCase n . assert

nameMatchTest (True ,p,s) = show p ++ " matches " ++ show s
nameMatchTest (False,p,s) = show p ++ " doesn't match " ++ show s

decompileCases =
   [ ("range-compression-1", "[*]",   "[*]")
   , ("range-compression-2", "[.]",   "[.]")
   , ("range-compression-3", "**[/]", "*[/]")
   , ("range-compression-4", "x[.]",  "x[.]")
   , ("range-compression-5", "[^~-]", "[^~-]")
   , ("range-compression-6", "[^!-]", "[^!-]")
   ]

matchCases =
   [ (True , "*"          , "")
   , (True , "**"         , "")
   , (True , "asdf"       , "asdf")
   , (True , "a*f"        , "asdf")
   , (True , "a??f"       , "asdf")
   , (True , "*"          , "asdf")
   , (True , "a*bc"       , "aXbaXbc")
   , (True , "a**bc"      , "aXbaXbc")
   , (False, "a*b"        , "aXc")
   , (True , "foo/bar.*"  , "foo/bar.baz")
   , (True , "foo/*.baz"  , "foo/bar.baz")
   , (False, "*bar.*"     , "foo/bar.baz")
   , (False, "*.baz"      , "foo/bar.baz")
   , (False, "foo*"       , "foo/bar.baz")
   , (False, "foo?bar.baz", "foo/bar.baz")
   , (True , "**/*.baz"   , "foo/bar.baz")
   , (True , "**/*"       , "foo/bar.baz")
   , (True , "**/*"       , "foo/bar/baz")
   , (True , "*/*.baz"    , "foo/bar.baz")
   , (True , "*/*"        , "foo/bar.baz")
   , (False, "*/*"        , "foo/bar/baz")
   , (False, "*.foo"      , ".bar.foo")
   , (False, "*.bar.foo"  , ".bar.foo")
   , (False, "?bar.foo"   , ".bar.foo")
   , (True , ".*.foo"     , ".bar.foo")
   , (True , ".*bar.foo"  , ".bar.foo")
   , (False, "foo.[ch]"   , "foo.a")
   , (True , "foo.[ch]"   , "foo.c")
   , (True , "foo.[ch]"   , "foo.h")
   , (False, "foo.[ch]"   , "foo.d")
   , (False, "foo.[c-h]"  , "foo.b")
   , (True , "foo.[c-h]"  , "foo.c")
   , (True , "foo.[c-h]"  , "foo.e")
   , (True , "foo.[c-h]"  , "foo.f")
   , (True , "foo.[c-h]"  , "foo.h")
   , (False, "foo.[c-h]"  , "foo.i")
   , (True , "<->3foo"    , "123foo")
   , (True , "<10-15>3foo", "123foo")
   , (True , "<0-5>23foo" , "123foo")
   , (True , "<94-200>foo", "123foo")
   , (False, "[.]x"       , ".x")
   , (False, "foo[/]bar"  , "foo/bar")
   , (False, "foo[,-0]bar", "foo/bar")
   , (True , "foo[,-0]bar", "foo.bar")
   , (True , "[]x]"       , "]")
   , (True , "[]x]"       , "x")
   , (False, "[b-a]"      , "a")
   , (False, "<4-3>"      , "3")
   , (True , "[]-b]"      , "]")
   , (False, "[]-b]"      , "-")
   , (True , "[]-b]"      , "b")
   , (True , "[]-b]"      , "a")
   , (True , "[]-]"       , "]")
   , (True , "[]-]"       , "-")
   , (True , "[#-[]"      , "&")
   , (False, "[^x]"       , "/")
   , (False, "[/]"        , "/")
   , (True , "a[^x]"      , "a.")
   , (True , "a[.]"       , "a.")
   , (False, ".//a"       , "/a")
   , (True,  ".//a"       , "a")
   , (True,  ".//a"       , "./a")
   , (True , ".*/a"       , "./a")
   , (True , ".*/a"       , "../a")
   , (True , ".*/a"       , ".foo/a")
   , (True , ".**/a"      , ".foo/a")
   , (False, ".**/a"      , "../a")
   , (False, ".**/a"      , "./a")
   , (False, ".**/a"      , "a")
   , (True , ".**/a"      , ".foo/a")
   , (True , "f**/a"      , "foo/a")
   , (True , "f**/"       , "f/")
   , (True , "f**/"       , "f///")
   , (True , "f**/x"      , "f///x")
   , (True , "f/"         , "f///")
   , (True , "f/x"        , "f///x")
   , (True , "[]"         , "[]")
   , (True , "[!]"        , "[!]")
   , (True , "[^]"        , "[^]")
   , (True , "[abc"       , "[abc")
   , (True , "<abc"       , "<abc")
   , (True , "<1-"        , "<1-")
   , (True , "[^-~]"      , "x")
   , (False, "[^-~]"      , "X")
   , (False, "[^^-~]"     , "x")
   , (False, "[^-]"       , "-")
   ]

matchWithCases =
   [ (True , compDefault, matchDefault { ignoreCase = True }, "[@-[]", "a")
   , (True , compPosix  , matchDefault                      , "a[/]b", "a[/]b")
   ]
