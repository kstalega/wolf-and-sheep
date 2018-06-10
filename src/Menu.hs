module Menu where

import Data.Char
import DataStructures
import Board
import Alg

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
                                                                          steerSheep state (getSheepIndexAsAInt sheepIndex) directionOfMovement

-- get sheep index as int
getSheepIndexAsAInt :: Char -> Int
getSheepIndexAsAInt x | isDigit x == True = digitToInt x
                      | otherwise = error "Index of sheep wrongly formattted"

moveSheep :: State -> Int -> Int -> IO()
moveSheep state sheepIndex directionOfMovement = do
                                                            if (elem p (possibleOneSheepMoves sheepIndex state))
                                                              then
                                                                putStrLn "Move is possible"
                                                            else 
                                                                putStrLn "Move is not possible"
                                                            where possibleMoves = possibleOneSheepMoves sheepIndex state
                                                                  xCurrent = xPoint ((sheepsPos state) !! sheepIndex)
                                                                  xNext = xCurrent + directionOfMovement
                                                                  yCurrent = yPoint ((sheepsPos state) !! sheepIndex)
                                                                  yNext = yCurrent + 1
                                                                  p = Point xNext yNext

-- steer sheep
steerSheep :: State -> Int -> Char -> IO()
steerSheep state sheepIndex directionOfMovement = do
                                              if elem sheepIndex [0..3]
                                              then case [directionOfMovement] of
                                                'L':_ -> moveSheep state sheepIndex (-1)
                                                'R':_ -> moveSheep state sheepIndex 1
                                                _ -> do
                                                      putStrLn "Wrong command 1"
                                              else
                                                putStrLn "Wrong command 2"