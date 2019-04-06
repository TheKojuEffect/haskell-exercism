module CryptoSquare (encode) where

import Data.Char
import Data.List
import Data.Ord

encode :: String -> String
encode = encrypt . padToSquare . normalize
    where
        normalize :: String -> String
        normalize = map toLower . filter isAlphaNum

        padToSquare :: String -> String
        padToSquare ys = rightPad (column ys ^ 2) ys

        column :: String -> Int
        column  = ceiling . sqrt . fromIntegral . length
        
        rightPad :: Int -> String -> String
        rightPad len ys = ys ++ replicate (len - length ys) ' '

        encrypt :: String -> String
        encrypt ys = unwords $ map (map fst) $ groupBy second $ sortOn snd $ positionZip ys

        positionZip :: String -> [(Char, Int)]
        positionZip ys = zip ys $ concat $ replicate (column ys) [0 .. (column ys - 1)]

        second :: (Char, Int) -> (Char, Int) -> Bool
        second t1 t2 = snd t1 == snd t2

        