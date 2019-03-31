module Clock (addDelta, fromHourMin, toString) where

import Text.Printf


data Clock = Clock Int Int
    deriving (
        Eq,
        Show
        )


fromHourMin :: Int -> Int -> Clock
fromHourMin hours minutes = Clock hh mm
        where
            hh = mod (hours + (div minutes 60)) 24
            mm = mod minutes 60

toString :: Clock -> String
toString (Clock hours minutes) = printf "%02d:%02d" hours minutes

addDelta :: Int -> Int -> Clock -> Clock
addDelta hours minutes (Clock hh mm) = fromHourMin (hh + hours) (mm + minutes)
