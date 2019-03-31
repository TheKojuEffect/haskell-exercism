module Anagram (anagramsFor) where

import Data.List
import Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = [ys | ys <- xss, lower xs /= lower ys, sortedLower xs == sortedLower ys]
                    where
                      sortedLower = sort.lower
                      lower = map toLower