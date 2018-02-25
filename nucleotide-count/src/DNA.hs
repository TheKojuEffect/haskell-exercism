module DNA (nucleotideCounts) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs
    | invalid xs == True = Left xs
    | xs == "" = Right empty
    | otherwise = Right $ foldl (\acc x -> Map.insert x (acc Map.! x + 1) acc) empty xs
    where
        invalid xs = not $ all (`elem` "ACGT") xs
        empty = Map.fromList [('A', 0), ('C', 0), ('G', 0), ('T', 0)]
