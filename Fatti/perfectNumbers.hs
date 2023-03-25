module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | sum (listDiv n) == n = Just Perfect
  | sum (listDiv n) < n  = Just Deficient
  | sum (listDiv n) > n  = Just Abundant
  | otherwise = Nothing

listDiv :: Integral a => a -> [a]
listDiv n = filter (\x -> rem n x == 0) [1..(div n 2)]