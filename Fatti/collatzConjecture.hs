module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = collatz' n 0

collatz' :: Integer -> Integer -> Maybe Integer
collatz' n c
  | n <= 0 = Nothing
  | n == 1 = Just c
  | even n = collatz' (div n 2) (c+1)
  | odd n  = collatz' (3*n+1) (c+1)

