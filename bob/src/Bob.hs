module Bob (responseFor) where

import Data.Char


isYelling stmt = any isAlpha stmt && all isUpper (filter isAlpha stmt)
isQuestion stmt = last (filter (not . isSpace) stmt) == '?'

responseFor :: String -> String
responseFor xs
    | all isSpace xs = "Fine. Be that way!"
    | isQuestion xs && isYelling xs = "Calm down, I know what I'm doing!"
    | isQuestion xs = "Sure."
    | isYelling xs = "Whoa, chill out!"
    | otherwise = "Whatever."

