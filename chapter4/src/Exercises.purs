module Exercises where

import Prelude

import Data.Array (null, filter, (..), length)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)


-- 4.4

-- 1
isEven :: Int -> Boolean
isEven n = 
  if n == 0
    then true
    else not isEven (n - 1)

-- 2
countEven :: Array Int -> Int
countEven nums =
  if null nums
     then 0
     else if isEven (unsafePartial head nums)
       then 1 + countEven (unsafePartial tail nums)
       else countEven (unsafePartial tail nums)

-- 4.7

-- 1
squareNumbers :: Array Number -> Array Number
squareNumbers = map (\n -> n * n)

-- 3
infix 4 filter as <$?>

-- 2
removeNegatives :: Array Number -> Array Number
removeNegatives arr = (\n -> n >= 0.0) <$?> arr

-- exercise 4.10 boilerplate

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

-- 4.10

-- 1
isPrime :: Int -> Boolean
isPrime n = (length (factors n)) /= 2

-- 2
cartesianProduct :: forall t. Array t -> Array t -> Array (Array t)
cartesianProduct xs ys = do
  i <- xs
  j <- ys
  pure [i, j]


