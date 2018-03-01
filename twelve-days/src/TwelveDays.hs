module TwelveDays (recite) where

recite :: Int -> Int -> [String]
recite start stop = foldl compose [] [start..stop]
    where
        compose lyrics day = lyrics ++ [(line day start)]
        line day start = prefix ++ (dayWords !! (day-1)) ++ middle ++ (totalGifts day) ++ "."

prefix = "On the "
middle = " day of Christmas my true love gave to me"

totalGifts 1 = ", " ++ toGift 1
totalGifts day = concatMap toGiftVerse (reverse [1..day])
    where
        toGiftVerse day = separator day ++ toGift day
        separator 1 = ", and "
        separator _ = ", "

toGift day = gifts !! (day - 1)

days = [1..12]
gifts = [
    "a Partridge in a Pear Tree",
    "two Turtle Doves",
    "three French Hens",
    "four Calling Birds",
    "five Gold Rings",
    "six Geese-a-Laying",
    "seven Swans-a-Swimming",
    "eight Maids-a-Milking",
    "nine Ladies Dancing",
    "ten Lords-a-Leaping",
    "eleven Pipers Piping",
    "twelve Drummers Drumming"]
dayWords = [
    "first",
    "second",
    "third",
    "fourth",
    "fifth",
    "sixth",
    "seventh",
    "eighth",
    "ninth",
    "tenth",
    "eleventh",
    "twelfth"]

