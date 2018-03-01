module ETL (transform) where

import Data.Map (Map, foldlWithKey, empty, insert, fromList)
import qualified Data.Map as Map
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform = Map.fromList . Map.foldlWithKey transform' []
    where
        transform' result point chars = result ++ map (`score` point) chars
        score char point = (toLower char, point)
