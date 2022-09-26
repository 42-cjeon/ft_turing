import System.IO
import System.Environment (getArgs)
import Parser (parse, jValueToTDesc, JValue(JObject), jValue, TDesc)
import Turing (runTM)

parseToTDesc s = case parse s of
  [(JObject json, "")] -> jValueToTDesc json
  _ -> Nothing

parseArgv :: [String] -> Either String (String, String)
parseArgv argv = case argv of
  [f, t] -> Right (f, t)
  [_] -> Left "parse error: machine's tape is missing"
  _ -> Left "usage: ft_turing <machine_decription.json> <machine input>"

run :: [String] -> IO ()
run argv =
  case parseArgv argv of
    Right (file, tape) -> do 
      fileContents <- readFile file
      case parseToTDesc fileContents of
        Nothing -> putStrLn "parse error: incomplate JSON format"
        Just desc -> runTM desc tape
    Left err -> putStrLn err

main :: IO ()
main =
  do
    args <- getArgs
    run args