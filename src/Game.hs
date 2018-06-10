module Game where

import System.IO
import Board
import DataStructures

defaultState :: State
defaultState = [(Point 3 6), (Point 0 1), (Point 3 0), (Point 5 0), (Point 7 0)]

-- new game
newGame :: IO()
newGame = do
            printBoard defaultState 0 0
