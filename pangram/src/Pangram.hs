module Pangram (isPangram) where

import Data.List (elem)
import Data.Char

containsCharacter x text = elem x text

upperCase text = map toUpper text
isUpperCasePangram upperCaseText = foldl (\pangram x -> pangram && containsCharacter x upperCaseText ) True ['A'..'Z']

isPangram :: String -> Bool
isPangram text = isUpperCasePangram (upperCase text)
