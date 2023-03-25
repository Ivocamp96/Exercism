module Pangram (isPangram) where
import Data.Char (toLower, isAlpha)

isPangram :: String -> Bool
isPangram txt = all (`elem` filtro txt) ['a'..'z']
  where filtro :: String -> String
        filtro = map toLower . filter isAlpha
