module ResistorColors
  ( Color(..)
  , Resistor(..)
  , label
  , ohms
  ) where

data Color
  = Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor
  { bands :: (Color, Color, Color)
  } deriving (Show)

ohms :: Resistor -> Int
ohms resistor = (tens * 10 + ones) * (10 ^ zeroes)
  where
    (first, second, third) = bands resistor
    tens = fromEnum first
    ones = fromEnum second
    zeroes = fromEnum third

label :: Resistor -> String
label resistor = show value ++ " " ++ unit
  where
    ohmsValue = ohms resistor
    (value, unit) = convert ohmsValue
    convert :: Int -> (Int, String)
    convert x
      | x >= 1000000000 = (quot x 1000000000, "gigaohms")
      | x >= 1000000 = (quot x 1000000, "megaohms")
      | x >= 1000 = (quot x 1000, "kiloohms")
      | otherwise = (x, "ohms")
