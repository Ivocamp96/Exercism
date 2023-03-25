module Isogram (isIsogram) where

import Data.Char
import Data.List

isIsogram :: String -> Bool
isIsogram = all ((== 1) . length)
          . group
          . sort 
          . filter isAlpha
          . map toLower