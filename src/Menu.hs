module Menu where

import DataStructures

processComands :: State -> IO()
processComands state = do
                  (operationID:commandSettings) <- fmap words getLine
                  case operationID of
                    '-':'m':_ -> do
                                  processSheepSteering state commandSettings
                                  putStrLn "Moving ship"
                                  processComands state
                    _ -> do
                          putStrLn "Wrong command"
                          processComands state

processSheepSteering :: State -> [String] -> IO()
processSheepSteering state (sheepIndex:directionOfMovement:_)  = do
                                                    putStrLn sheepIndex
                                                    putStrLn directionOfMovement