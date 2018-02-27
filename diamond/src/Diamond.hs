module Diamond (diamond) where

import Data.Maybe
import Data.Char
import Data.List

diamond :: Char -> Maybe [String]
diamond ch
    | isAlpha ch = Just $ diamond' ch
    | otherwise = Nothing

diamondRow char charIndex charsLength
    | char == 'A' = around ++ ['A'] ++ around
    | otherwise = around ++ [char] ++ middle ++ [char] ++ around
        where
            spaces = repeat ' '
            around = take (charsLength - charIndex - 1) spaces
            middle = take (2 * charIndex - 1) spaces

diamond' :: Char -> [String]
diamond' ch = foldr helper [] chars
            where
                chars = ['A'..ch]
                charsLength = length chars
                padBoth y ys = y : ys ++ [y]
                row c = diamondRow c (fromJust $ elemIndex c chars) charsLength
                helper c result
                    | c == ch = [row c]
                    | otherwise = padBoth (row c) result
