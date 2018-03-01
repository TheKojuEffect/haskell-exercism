module Phone (number) where

import Data.Char (isNumber)

number :: String -> Maybe String
number = number' . (filter isNumber)
    where
        valid10DigitNumber num@(area1: area2: area3: exchange1: remaining)
            | (elem area1 ['2'..'9']) && (elem exchange1 ['2'..'9']) = Just num
            | otherwise = Nothing
        number' xs
            | xs == "" = Nothing
            | length xs == 11 && head xs == '1' = valid10DigitNumber $ tail xs
            | length xs == 10 = valid10DigitNumber xs
            | otherwise = Nothing
