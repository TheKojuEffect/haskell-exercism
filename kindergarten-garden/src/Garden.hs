module Garden
  ( Plant(..)
  , garden
  , lookupPlants
  ) where

import Data.List
import Data.Maybe

data Plant
  = Clover
  | Grass
  | Radishes
  | Violets
  deriving (Eq, Show)

type Student = String

type Garden = [(Student, [Plant])]

garden :: [Student] -> String -> Garden
garden students plants = zipWith toGarden students allStudentPlants
  where
    toGarden student studentPlants = (student, studentPlants)
    allStudentPlants = map readPlants row
    row = zipWith (++) (head rows) (rows !! 1)
    rows = map (chunksOf 2) $ lines plants

lookupPlants :: Student -> Garden -> [Plant]
lookupPlants student garden = maybe [] snd $ find (\g -> student == fst g) garden

readPlant :: Char -> Plant
readPlant 'C' = Clover
readPlant 'G' = Grass
readPlant 'R' = Radishes
readPlant 'V' = Violets
readPlant _   = error "Invalid Plant"

readPlants :: [Char] -> [Plant]
readPlants = map readPlant

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
