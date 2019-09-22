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
value = foldl (\n c -> n * 10 + fromEnum c) 0
