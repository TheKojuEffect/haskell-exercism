module ProteinTranslation
  ( proteins
  ) where

import           Data.Maybe (catMaybes, isNothing)

proteins :: String -> Maybe [String]
proteins rna = Just $ catMaybes maybeProteins
  where
    codons = map read $ chunksOf 3 rna
    maybeProteinsWithStop = map toProtein codons
    maybeProteins = takeUntil isNothing maybeProteinsWithStop

data Codon
  = AUG
  | UUU
  | UUC
  | UUA
  | UUG
  | UCU
  | UCC
  | UCA
  | UCG
  | UAU
  | UAC
  | UGU
  | UGC
  | UGG
  | UAA
  | UAG
  | UGA
  deriving (Read)

toProtein :: Codon -> Maybe String
toProtein AUG = Just "Methionine"
toProtein UUU = Just "Phenylalanine"
toProtein UUC = Just "Phenylalanine"
toProtein UUA = Just "Leucine"
toProtein UUG = Just "Leucine"
toProtein UCU = Just "Serine"
toProtein UCC = Just "Serine"
toProtein UCA = Just "Serine"
toProtein UCG = Just "Serine"
toProtein UAU = Just "Tyrosine"
toProtein UAC = Just "Tyrosine"
toProtein UGU = Just "Cysteine"
toProtein UGC = Just "Cysteine"
toProtein UGG = Just "Tryptophan"
toProtein UAA = Nothing
toProtein UAG = Nothing
toProtein UGA = Nothing

chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil test (x:xs) =
  if test x
    then []
    else x : takeUntil test xs
