module Robot
  ( Bearing(East, North, South, West)
  , bearing
  , coordinates
  , mkRobot
  , move
  ) where

data Bearing
  = North
  | East
  | South
  | West
  deriving (Eq, Show, Enum, Bounded)

type Coordinate = (Integer, Integer)

type Robot = (Bearing, Coordinate)

bearing :: Robot -> Bearing
bearing (b, _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (_, c) = c

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot b (x, y) = (b, (x, y))

move :: Robot -> String -> Robot
move r instructions = foldl (\r1 i -> i r1) r moves
  where
    moves = map toInstruction instructions

toInstruction :: Char -> Robot -> Robot
toInstruction 'A' = moveAhead
toInstruction 'R' = moveRight
toInstruction 'L' = moveLeft
toInstruction _   = error "Unknown Instruction"

moveAhead :: Robot -> Robot
moveAhead r@(b, _) = (b, advance r)

advance :: Robot -> Coordinate
advance (North, (x, y)) = (x, y + 1)
advance (East, (x, y))  = (x + 1, y)
advance (South, (x, y)) = (x, y - 1)
advance (West, (x, y))  = (x - 1, y)

moveLeft :: Robot -> Robot
moveLeft (b, (x, y)) = (left b, (x, y))

left :: Bearing -> Bearing
left b
  | b == (minBound :: Bearing) = maxBound :: Bearing
  | otherwise = pred b

moveRight :: Robot -> Robot
moveRight (b, (x, y)) = (right b, (x, y))

right :: Bearing -> Bearing
right b
  | b == (maxBound :: Bearing) = minBound :: Bearing
  | otherwise = succ b
