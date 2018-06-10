module Menu where

import Data.Char
import DataStructures
import Board

-- process comands during the game
processComands :: State -> IO()
processComands state = do
                  (operationID:commandSettings) <- fmap words getLine
                  case operationID of
                    '-':'m':_ -> do
                                  processSheepSteering state commandSettings
                                  putStrLn "Moving sheep"
                                  printBoard state 0 0
                                  processComands state
                    _ -> do
                          putStrLn "Wrong command"
                          processComands state

-- process sheep steering command
processSheepSteering :: State -> [String] -> IO()
processSheepSteering state ((sheepIndex:_):(directionOfMovement:_):_)  = do
                                                                          steerSheep (getSheepIndexAsAInt sheepIndex) directionOfMovement

-- get sheep index as int
getSheepIndexAsAInt :: Char -> Int
getSheepIndexAsAInt x | isDigit x == True = digitToInt x
                      | otherwise = error "Index of sheep wrongly formattted"

-- steer sheep
steerSheep :: Int -> Char -> IO()
steerSheep sheepIndex directionOfMovement = do
                                              putStrLn (show sheepIndex)
                                              putStrLn [directionOfMovement]
