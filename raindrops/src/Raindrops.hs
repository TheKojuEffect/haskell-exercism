module Raindrops (convert) where

import Data.Maybe

convert :: Int -> String
convert n = case convert' n of
        "" -> show n
        rain -> rain

convert' = concat . catMaybes . (map raindrop). factors

factors n = [x | x <- [1..n], mod n x == 0]

raindrop 3 = Just "Pling"
raindrop 5 = Just "Plang"
raindrop 7 = Just "Plong"
raindrop _ = Nothing