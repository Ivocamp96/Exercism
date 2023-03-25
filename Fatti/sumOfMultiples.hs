{- Given a number, find the sum of all the unique multiples of particular numbers up to but not including that number.
If we list all the natural numbers below 20 that are multiples of 3 or 5, we get 3, 5, 6, 9, 10, 12, 15, and 18.
The sum of these multiples is 78.-}

module SumOfMultiples (sumOfMultiples) where
import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum
                             . map head
                             . group
                             . sort
                             . concatMap (makeList limit)
                             $ factors

makeList :: Integer -> Integer -> [Integer]
makeList limit factor 
  | factor == 0 = [0]
  | otherwise = filter (\n -> mod n factor == 0) [1..(limit-1)]