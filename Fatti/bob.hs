module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor xs
  | isSilence xs               = "Fine. Be that way!"
  | isQuestion xs && isYell xs = "Calm down, I know what I'm doing!"
  | isQuestion xs              = "Sure."
  | isYell xs                  = "Whoa, chill out!"
  | otherwise                  = "Whatever."

isQuestion :: String -> Bool
isQuestion [] = False
isQuestion xs = last (filter (/= ' ') xs) == '?'

isYell :: String -> Bool
isYell xs = myFilter $ filter isAlpha xs
  where myFilter n
          | n == ""       = False
          | all isUpper n = True
          | otherwise     = False

isSilence :: String -> Bool
isSilence xs = (xs == "") || all isSpace xs