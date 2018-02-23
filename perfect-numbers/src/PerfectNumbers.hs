module PerfectNumbers (classify, Classification(..)) where

import Data.Maybe

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

factors :: Int -> Maybe [Int]
factors n
    | n > 0 = Just [x | x <- [1..n-1], n `mod` x == 0]
    | otherwise = Nothing


aliquotSum :: Int -> Maybe Int
aliquotSum n
        | isNothing $ factors n =  Nothing
        | otherwise = Just $ sum $ fromJust (factors n)


classify :: Int -> Maybe Classification
classify n
        | isNothing $ aliquotSum n = Nothing
        | n < (fromJust $ aliquotSum n) = Just Abundant
        | n == (fromJust $ aliquotSum n) = Just Perfect
        | n > (fromJust $ aliquotSum n) = Just Deficient
        | otherwise = Nothing