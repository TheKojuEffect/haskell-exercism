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
  deriving (Read)

sumOf :: Int -> [Int] -> Int
sumOf n nums = sum $ filter (== n) nums

yacht :: Category -> [Int] -> Int
yacht Ones dices = sumOf 1 dices
yacht Twos dices = sumOf 2 dices
yacht Threes dices = sumOf 3 dices
yacht Fours dices = sumOf 4 dices
yacht Fives dices = sumOf 5 dices
yacht Sixes dices = sumOf 6 dices
yacht FullHouse dices =
  if isFullHouse
    then total
    else 0
  where
    grouped = group $ sort dices
    isFullHouse = length grouped == 2 && has2or3 (head grouped)
    has2or3 xs = length xs == 2 || length xs == 3
    total = sum dices
yacht FourOfAKind dices = fourOfAKindSum
  where
    grouped = group $ sort dices
    fourKinds = maybe [] (take 4) $ find (\g -> length g >= 4) grouped
    fourOfAKindSum = sum fourKinds
yacht LittleStraight dices =
  if [1, 2, 3, 4, 5] == sort dices
    then 30
    else 0
yacht BigStraight dices =
  if [2, 3, 4, 5, 6] == sort dices
    then 30
    else 0
yacht Choice dices = sum dices
yacht Yacht dices =
  if all (== head dices) (tail dices)
    then 50
    else 0
