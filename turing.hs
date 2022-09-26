module Turing where

import Data.List (find)
import qualified Data.Map.Strict as Map
import Parser (TAction (LEFT, RIGHT), TDesc, action, blank, finals, initial, toState, transitions, write)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth index with (x : xs) =
  if index == 0
    then with : xs
    else x : replaceNth (index - 1) with xs

getTransMap desc state = case Map.lookup state $ transitions desc of
  Nothing -> case find (== state) (finals desc) of
    Just fin -> Left ("HALT: final state (" ++ fin ++ ") detected")
    Nothing -> Left ("HALT: unknown state (" ++ state ++ ") detected")
  Just x -> Right x

getFunc value state transMap = case Map.lookup value transMap of
  Nothing -> Left ("error: cannot find transition from (state=" ++ state ++ ", tape_value=" ++ value ++ ")")
  Just x -> Right x

lookUpTransition desc tape state index =
  do
    transMap <- getTransMap desc state
    func <- getFunc value state transMap
    pure (transMap, func)
  where
    value = [tape !! index]

applyTransition desc tape state index = case lookUpTransition desc tape state index of
  Left msg -> Left msg
  Right (map, func) -> Right (desc, tape'', state', nextIndex')
    where
      len = length tape
      value = [tape !! index]
      nextIndex = case action func of
        LEFT -> index - 1
        RIGHT -> index + 1
      tape' = replaceNth index (head (write func)) tape
      tape'' = case (nextIndex < 0, nextIndex >= len) of
        (False, False) -> tape'
        (True, False) -> head (blank desc) : tape'
        (False, True) -> tape' ++ [head $ blank desc]
        (True, True) -> error "length of tape is less then zero"
      nextIndex' =
        if nextIndex < 0
          then 0
          else nextIndex
      state' = toState func

runTM :: TDesc -> String -> IO ()
runTM desc tape = runTMRec desc tape (initial desc) 0 0

runTMRec :: TDesc -> String -> String -> Int -> Int -> IO ()
runTMRec desc tape state index count =
  do
    printCurrentState tape state index count
    case applyTransition desc tape state index of
      Left str -> putStrLn ("ft_turing: " ++ str)
      Right (desc', tape', state', index') -> runTMRec desc' tape' state' index' (count + 1)

printCurrentState tape state index count =
  do
    putStrLn ("-- " ++ "step: " ++ show count ++ " --")
    putStrLn ("current state: " ++ state)
    putStrLn (front ++ "\ESC[34m" ++ [value] ++ "\ESC[0m" ++ rear)
    putStrLn "--"

  where
    value = tape !! index
    front = take index tape
    rear = drop (index + 1) tape
