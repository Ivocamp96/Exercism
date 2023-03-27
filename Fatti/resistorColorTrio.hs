{-# LANGUAGE NumDecimals #-}

module ResistorColors (Color(..), Resistor(..), label, ohms) where

data Color =
    Black  -- 0
  | Brown  -- 1
  | Red    -- 2
  | Orange -- 3
  | Yellow -- 4
  | Green  -- 5
  | Blue   -- 6
  | Violet -- 7
  | Grey   -- 8
  | White  -- 9
  deriving (Show, Enum, Bounded, Eq)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

label :: Resistor -> String
label resistor
  | ohms resistor <= 999 = show (ohms resistor) ++ " ohms"
  | ohms resistor >= 1000 && ohms resistor < 10^6 = show (ohms resistor `div` 1000) ++ " kiloohms"
  | ohms resistor >= 10^6 && ohms resistor < 10^9 = show (ohms resistor `div` 10^6) ++ " megaohms"
  | ohms resistor >= 10^9 = show (ohms resistor `div` 10^9) ++ " gigaohms"



ohms :: Resistor -> Int
ohms = (\(x,y,z) -> (x * 10 + y) * z)
     . (\(x,y,z) -> ( numberize x
                    , numberize y
                    , 10 ^ numberize z ))
     . bands

numberize :: Color -> Int
numberize n
  | n == Black  = 0
  | n == Brown  = 1
  | n == Red    = 2
  | n == Orange = 3
  | n == Yellow = 4
  | n == Green  = 5
  | n == Blue   = 6
  | n == Violet = 7
  | n == Grey   = 8
  | n == White  = 9