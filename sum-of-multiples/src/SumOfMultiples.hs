module SumOfMultiples (sumOfMultiples) where

isMultiple x factors = any (isFactor x) factors
    where isFactor num factor = mod num factor == 0

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = foldl (+) 0 multiples
    where multiples = [x | x <- [1..limit-1], (isMultiple x factors)]

