module Isogram (isIsogram) where

import Data.Char

isIsogram :: String -> Bool
isIsogram "" = True
isIsogram (x:xs) = (ignoredChar x || isAbsent x upper) && isIsogram upper
    where
        upper = map toUpper xs
        ignoredChar x = x == ' ' || x == '-'
        isAbsent x xs = not (elem (toUpper x) xs)