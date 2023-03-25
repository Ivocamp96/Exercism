module Raindrops (convert) where

import Data.Maybe

convert :: Int -> String
convert n
  | convert' n == Just "" = show n
  | otherwise  = fromJust $ convert' n

convert' :: Int -> Maybe String
convert' = foldr (<>) (Just "")
         . filter isJust
         . sequence [ \n -> if rem n 3 == 0 then Just "Pling" else Nothing
                    , \n -> if rem n 5 == 0 then Just "Plang" else Nothing
                    , \n -> if rem n 7 == 0 then Just "Plong" else Nothing ]