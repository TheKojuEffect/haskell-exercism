module ArmstrongNumbers
  ( armstrong
  ) where

armstrong :: Integral a => a -> Bool
armstrong num = num == sum [d ^ len | d <- digits]
  where
    len = length digits
    digits = toDigits num
    toDigits 0 = []
    toDigits n = reminder : toDigits quotient
      where (quotient, reminder) = n `divMod` 10

