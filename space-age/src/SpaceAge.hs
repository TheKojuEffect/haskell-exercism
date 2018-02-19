module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

secondsIn1EarthYear = 31557600

secondsIn1YearOn :: Planet -> Float
secondsIn1YearOn Mercury = 0.2408467 * secondsIn1EarthYear
secondsIn1YearOn Venus = 0.61519726 * secondsIn1EarthYear
secondsIn1YearOn Earth = 1 * secondsIn1EarthYear
secondsIn1YearOn Mars =  1.8808158 * secondsIn1EarthYear
secondsIn1YearOn Jupiter =  11.862615 * secondsIn1EarthYear
secondsIn1YearOn Saturn =  29.447498 * secondsIn1EarthYear
secondsIn1YearOn Uranus =  84.016846 * secondsIn1EarthYear
secondsIn1YearOn Neptune =  164.79132 * secondsIn1EarthYear


ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / secondsIn1YearOn planet
