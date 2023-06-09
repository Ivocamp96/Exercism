module Acronym (abbreviate) where
import Data.Char (toUpper)

abbreviate :: String -> String
abbreviate xs = map (\(n:_) -> toUpper n) $ words xs