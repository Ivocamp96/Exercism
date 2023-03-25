module Acronym (abbreviate) where
import Data.Char

pulisci :: [Char] -> [Char]
pulisci [] = []
pulisci (x:xs)
  | isAlpha x = x   : pulisci xs
  | x == '\'' =       pulisci xs
  | otherwise = ' ' : pulisci xs

separa :: [Char] -> [Char]
separa [] = []
separa [x] = [x]
separa (x:y:ys)
    | isUpper y && isLower x = x : ' ' : y : separa ys
    | otherwise              = x : y : separa ys

abbreviate :: String -> String
abbreviate = map (toUpper . head)
           . concatMap (words . separa)
           . words
           . pulisci