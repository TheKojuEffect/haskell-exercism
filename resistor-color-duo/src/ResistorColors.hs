module ResistorColors
  ( Color(..)
  , value
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
  deriving (Eq, Show, Read, Enum)

value :: [Color] -> Int
value = resisterValue . reverse
  where
    resisterValue []     = 0
    resisterValue (c:cs) = 10 * value cs + fromEnum c
