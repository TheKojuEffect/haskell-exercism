module Scrabble (scoreLetter, scoreWord) where

import Data.List (elem)
import Data.Char (toUpper)

scoreLetter :: Char -> Integer
scoreLetter letter
        | isIn "AEIOULNRST" = 1
        | isIn "DG"         = 2
        | isIn "BCMP"       = 3
        | isIn "FHVWY"      = 4
        | isIn "K"          = 5
        | isIn "JX"         = 8
        | isIn "QZ"         = 10
        | otherwise         = 0
        where isIn = elem $ toUpper letter

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
