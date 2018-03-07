module RunLength (decode, encode) where

import Data.List
import Data.Char

decode :: String -> String
decode = fst . foldl decoder ("", Nothing) . groupBy isNumbers
    where
        isNumbers x y = isNumber x && isNumber y
        decoder (result, Nothing) segment
            | isNumber (head segment) =  (result, Just (read segment :: Int))
            | otherwise = (result ++ segment, Nothing)

        decoder (result, Just previousNumber) segment = (result ++ (replicate previousNumber $ head segment), Nothing)


encode :: String -> String
encode  = concatMap encoder . group
    where
        encoder (x:"") = [x]
        encoder segment@(x:_) = (show $ length segment) ++ [x]
