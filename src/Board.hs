module Board where

import DataStructures

-- wolf ascii representation
wolfChar = "w"

-- sheep ascii representation
sheepChar = "s"

-- black field
blackChar = "-"

-- printing board based on input date
printBoard :: State -> Int -> Int -> IO ()
printBoard state i j  | j < 8 = do 
                                    putStrLn ((printRow state i j) ++ "|")
                                    printBoard state 0 (j + 1)
                      | otherwise = putStrLn ""

-- printing one row of board
printRow :: State -> Int -> Int -> String
printRow state i j | i < 8 = "|" ++ (printField state i j) ++ (printRow state (i + 1) j)
                   | otherwise = ""

-- printing field of board
printField :: State -> Int -> Int -> String
printField state i j | (j `mod` 2 == 0) && (i `mod` 2 == 0) = "_"
                     | (j `mod` 2 /= 0) && (i `mod` 2 /= 0) = "_"
                     | otherwise = drawAnimalIfThereIsOne state i j

-- draw animal on board, if there is one
drawAnimalIfThereIsOne :: State -> Int -> Int -> String
-- if current column and row has wolf, draw it
drawAnimalIfThereIsOne ((Point wolfX wolfY):sheeps) i j | (i == wolfX && j == wolfY) = wolfChar
                                                    | otherwise = drawSheep sheeps i j

-- draw sheeps
drawSheep :: [Point] -> Int -> Int -> String
drawSheep [] _ _ = blackChar
drawSheep ((Point sheepX sheepY):remainingSheeps) boardX boardY | (sheepX == boardX && sheepY == boardY) = sheepChar 
                                                                | otherwise = drawSheep remainingSheeps boardX boardY