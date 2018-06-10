module Board where

import DataStructures

-- printing board based on input date
printBoard :: State -> Int -> Int -> IO ()
printBoard state i j  | j < 8 = do 
                                    putStrLn ("|" ++ (printRow state i j) ++ "|")
                                    printBoard state 0 (j+1)
                      | otherwise = putStrLn ""

-- printing one row of board
printRow :: State -> Int -> Int -> String
printRow state i j | i < 8 = (printField state i j) ++ (printField state (i+1) j)
                   | otherwise = ""

-- printing field of board
printField :: State -> Int -> Int -> String
printField state i j | (j `mod` 2 == 0) && (i `mod` 2 == 0) = "_"
                     | (j `mod` 2 /= 0) && (i `mod` 2 /= 0) = "_"
                     | otherwise = "*"