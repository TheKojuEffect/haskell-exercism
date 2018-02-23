module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
    | length xs /= length ys = Nothing
    | otherwise = Just $ hamming 0 xs ys
        where
            hamming d [] [] = d
            hamming d (x:xs') (y: ys')
                | x == y = hamming d xs' ys'
                | otherwise = hamming (d+1) xs' ys'
