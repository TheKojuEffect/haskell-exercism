module ArmstrongNumbers
  ( armstrong
  ) where

import           Data.Char

armstrong :: Int -> Bool
armstrong n = n == calculate num 0
  where
    num = show n
    d = length num
    calculate :: String -> Int -> Int
    calculate [] total     = total
    calculate (x:xs) total = calculate xs $ total + digitToInt x ^ d
