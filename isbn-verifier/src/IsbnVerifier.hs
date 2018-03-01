module IsbnVerifier (isbn) where

import Data.Char (isNumber, digitToInt)
import Control.Monad (sequence)

isbn :: String -> Bool
isbn xs@(x1:'-':x2:x3:x4:'-':x5:x6:x7:x8:x9:'-':x10) = isbn $ filter (/= '-') xs
isbn xs@(x1:x2:x3:x4:x5:x6:x7:x8:x9:x10) = validIsbnSum xs
    where
        validIsbnSum xs = case isbnSum xs of
            Just total -> total `mod` 11 == 0
            Nothing -> False
        isbnSum xs = fmap sum $ sequence $ map isbnValue (zip [0..] xs)
        isbnValue (9, 'X') = Just 10
        isbnValue (index, digit)
            | isNumber digit = Just $ (digitToInt digit) * (10 - index)
            | otherwise = Nothing

isbn _ = False
