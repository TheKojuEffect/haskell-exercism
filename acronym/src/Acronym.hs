module Acronym (abbreviate) where

import Data.Char

append xs x = xs++[toUpper x]

abbreviate :: String -> String
abbreviate (x:xs) =  acronym [x] x xs
    where
        acronym :: String -> Char -> String -> String
        acronym result _ [] = result
        acronym result lastChar (y:ys)
            | lastChar == ' ' || lastChar == '-' = acronym (append result y) y ys
            | otherwise = if isUpper y && isLower lastChar
                            then acronym (append result y) y ys
                            else acronym result y ys
