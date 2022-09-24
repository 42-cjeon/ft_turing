module Turing where

import Parser (TDesc, transitions, initial, write, action, toState, blank, TAction(LEFT, RIGHT))
import qualified Data.Map.Strict as Map

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth index with (x : xs) =
  if index == 0
    then with : xs
    else x : replaceNth (index - 1) with xs

runTM :: TDesc -> String -> IO()
runTM desc tape = runTMRec desc tape (initial desc) 0

runTMRec :: TDesc -> String -> String -> Int -> IO()
runTMRec desc tape state index = 
  do
    putStrLn tape
    runTMRec desc tape'' state' nextIndex'
  where
    len = length tape
    transMap = case Map.lookup state $ transitions desc of

      Nothing -> error ("HALT : unknown state=" ++ state ++ " detected")
      Just transMap' -> transMap'
    value = [tape !! index]
    func = case Map.lookup value transMap of
      Nothing -> error ("HALT : unknown (tape_value=" ++ value ++ ", state=" ++ state ++ ") pair detected")
      Just func' -> func'
    tape' = replaceNth index (head (write func)) tape
    nextIndex = case action func of
      LEFT -> index - 1
      RIGHT -> index + 1
    tape'' = case (nextIndex < 0, nextIndex >= len) of
      (False, False)  -> tape'
      (True, False) -> [head $ blank desc] ++ tape'
      (False, True) -> tape' ++ [head $ blank desc]
    nextIndex' = if nextIndex < 0
      then 0
      else nextIndex
    state' = toState func