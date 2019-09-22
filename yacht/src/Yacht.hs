module Yacht
  ( yacht
  , Category(..)
  ) where

import           Data.List

data Category
  = Ones
  | Twos
  | Threes
  | Fours
  | Fives
  | Sixes
  | FullHouse
  | FourOfAKind
  | LittleStraight
  | BigStraight
  | Choice
  | Yacht

sumOf :: Int -> [Int] -> Int
sumOf n nums = sum $ filter (== n) nums

fullHouse :: [Int] -> Int
fullHouse dices
  | isFullHouse = sum dices
  | otherwise = 0
  where
    isFullHouse = (== [2, 3]) $ sort $ map length $ group $ sort dices

fourOfAKind :: [Int] -> Int
fourOfAKind dices = fourOfAKindSum
  where
    fourOfAKindSum = sum fourKinds
    fourKinds = maybe [] (take 4) $ find (\g -> length g >= 4) grouped
    grouped = group $ sort dices

littleStraight :: [Int] -> Int
littleStraight dices
  | [1 .. 5] == sort dices = 30
  | otherwise = 0

bigStraight :: [Int] -> Int
bigStraight dices
  | [2 .. 6] == sort dices = 30
  | otherwise = 0


yacht':: [Int] -> Int
yacht' dices =
  if 1 == length (group dices)
    then 50
    else 0

yacht :: Category -> [Int] -> Int
yacht Ones           = sumOf 1
yacht Twos           = sumOf 2
yacht Threes         = sumOf 3
yacht Fours          = sumOf 4
yacht Fives          = sumOf 5
yacht Sixes          = sumOf 6
yacht FullHouse      = fullHouse
yacht FourOfAKind    = fourOfAKind
yacht LittleStraight = littleStraight
yacht BigStraight    = bigStraight
yacht Choice         = sum
yacht Yacht          = yacht'
