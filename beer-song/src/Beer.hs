module Beer (song) where

import Data.List

firstRhyme 0 = "No more bottles of beer on the wall, "
firstRhyme 1 = "1 bottle of beer on the wall, "
firstRhyme n = show n ++ " bottles of beer on the wall, "

secondRhyme 0 = "no more bottles of beer.\n"
secondRhyme 1 = "1 bottle of beer.\n"
secondRhyme n = show n ++ " bottles of beer.\n"

thirdRhyme 0 = "Go to the store and buy some more, "
thirdRhyme 1 = "Take it down and pass it around, "
thirdRhyme n = "Take one down and pass it around, "

forthRhyme 0 = "99 bottles of beer on the wall.\n"
forthRhyme 1 = "no more bottles of beer on the wall.\n"
forthRhyme 2 = "1 bottle of beer on the wall.\n"
forthRhyme n = show (n - 1) ++ " bottles of beer on the wall.\n"

verse n = firstRhyme n ++ secondRhyme n ++ thirdRhyme n ++ forthRhyme n

song :: String
song = intercalate "\n" $ map verse [99,98..0]

