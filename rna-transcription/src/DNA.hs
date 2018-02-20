module DNA (toRNA) where

transcribe x = case x of
    'G' -> 'C'
    'C' -> 'G'
    'T' -> 'A'
    'A' -> 'U'

toRNA :: String -> Maybe String
toRNA xs = if (all (`elem` "GCTA") xs) then Just (map transcribe xs) else Nothing
