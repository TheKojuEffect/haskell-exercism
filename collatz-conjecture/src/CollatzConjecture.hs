module CollatzConjecture (collatz) where

isEven n = mod n 2 == 0
isOdd n = not $ isEven n

evenCollatz n = quot n 2
oddCollatz n = 3 * n + 1

collatzLoop :: Integer -> Integer
collatzLoop n
    | isEven n = evenCollatz n
    | isOdd n = oddCollatz n

collatz :: Integer -> Maybe Integer
collatz n = loop n 0 where
        loop n' s'
            | n' <= 0 = Nothing
            | n' == 1 = Just s'
            | otherwise = loop (collatzLoop n') (s' + 1)


