import System.IO
import System.Environment
import Parser (parse, jValueToTDesc, JValue(JObject), jValue)
import Turing (runTM)

parseToTDesc s = case parsed of
  [(JObject json, "")] -> jValueToTDesc json
  _ -> Nothing
  where parsed = parse s

-- test = 
--   do
--     s <- readFile "example.json"
--     pure (parse jValue s)

main :: IO ()
main =
  do
    args <- getArgs
    s <- readFile $ head args
    case parseToTDesc s of
      Nothing -> error "failed to parse"
      Just x -> runTM x "111-11="