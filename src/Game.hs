module Game where

import System.IO
import Board

-- new game
newGame :: IO()
newGame = do
            printBoard