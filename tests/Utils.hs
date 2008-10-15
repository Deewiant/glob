-- File created: 2008-10-15 20:50:31

module Utils where

fromRight (Right x) = x
fromRight _         = error "fromRight :: Left"

isRight (Right _) = True
isRight _         = False

a --> b = not a || b
