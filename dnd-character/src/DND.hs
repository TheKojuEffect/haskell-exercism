module DND
  ( Character(..)
  , ability
  , modifier
  , character
  ) where

import           Data.List       (sort)
import           Test.QuickCheck (Gen, choose, vectorOf)

data Character = Character
  { strength     :: Int
  , dexterity    :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom       :: Int
  , charisma     :: Int
  , hitpoints    :: Int
  } deriving (Show, Eq)

modifier :: Int -> Int
modifier c = (c - 10) `div` 2

ability :: Gen Int
ability = fmap (sum . tail . sort) (vectorOf 4 dice)

dice :: Gen Int
dice = choose (1, 6)

character :: Gen Character
character = do
  strength' <- ability
  dexterity' <- ability
  constitution' <- ability
  intelligence' <- ability
  wisdom' <- ability
  charisma' <- ability
  return
    Character
      { strength = strength'
      , dexterity = dexterity'
      , constitution = constitution'
      , intelligence = intelligence'
      , wisdom = wisdom'
      , charisma = charisma'
      , hitpoints = 10 + modifier constitution'
      }
