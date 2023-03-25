module Triangle (TriangleType(..), triangleType) where

import Data.List

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c = triangleFilter $ nub $ sort [a, b, c]

triangleFilter l
    | length l == 1 && head l == 0 = Illegal
    | length l == 1 = Equilateral
    | length l == 2 && (2 * head l) > last l = Isosceles
    | length l == 2 && (2 * head l) <= last l = Illegal
    | head l + l !! 1 > l !! 2 = Scalene
    | otherwise = Illegal