import System.IO
import System.Environment
import Parser (parse, jValueToTDesc, JValue(JObject), jValue)

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
    print (parseToTDesc s)