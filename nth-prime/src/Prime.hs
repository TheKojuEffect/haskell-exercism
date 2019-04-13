module Prime
  ( nth
  ) where
-- nth prime number in haskell
-- Implementation very similar to algorithm: https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
nth :: Int -> Maybe Integer
nth n
  | n < 1 = Nothing
  | otherwise = Just $ primes [2 ..] !! (n - 1)

primes :: [Integer] -> [Integer]
primes []     = []
primes (x:xs) = x : primes [p | p <- xs, p `rem` x > 0]
