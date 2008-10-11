-- File created: 2008-10-10 22:03:00

module Base (PString(unPS), Path(unP), fromRight, isRight) where

import Test.QuickCheck

import System.FilePath (extSeparator, pathSeparator)
import System.FilePath.Glob.Base

import Utils (fromRight, isRight)

newtype PString = PatString { unPS :: String } deriving Show
newtype Path    = Path      { unP  :: String } deriving Show

alpha = extSeparator : pathSeparator : "-" ++ ['a'..'z'] ++ ['0'..'9']

instance Arbitrary PString where
   arbitrary = sized $ \size -> do
      let xs =
             (1, return "**/") :
             map (\(a,b) -> (a*100,b))
             [ (40, plain)
             , (20, return "?")
             , (20, charRange)
             , (10, return "*")
             , (10, openRange)
             ]

      s <- mapM (const $ frequency xs) [1..size]
      return.PatString $ concat s

instance Arbitrary Path where
   arbitrary = sized $ \size -> do
      s <- mapM (const plain) [1..size `mod` 16]
      return.Path $ concat s

plain = sized $ \size -> do
   s <- mapM (const $ elements alpha) [0..size `mod` 3]
   return s

charRange = do
   s <- plain
   return$ "[" ++ s ++ "]"

openRange = do
   probA <- choose (0,1) :: Gen Float
   probB <- choose (0,1) :: Gen Float
   a <- if probA > 0.4
           then fmap (Just .abs) arbitrary
           else return Nothing
   b <- if probB > 0.4
           then fmap (Just .abs) arbitrary
           else return Nothing
   return.concat $
      [ "<"
      , maybe "" show (a :: Maybe Int)
      , "-"
      , maybe "" show (b :: Maybe Int)
      , ">"
      ]
