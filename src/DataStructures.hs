module DataStructures where

-- board position
data Point = Point Int Int deriving (Show)

-- get cordinates info
xPoint :: Point -> Int
xPoint (Point x _) = x

yPoint :: Point -> Int
yPoint (Point _ y) = y

-- state of the game
-- first point is for wolf
-- 2nd - 5th for sheeps
type State = [Point]
