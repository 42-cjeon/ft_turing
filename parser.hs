import Data.Char (isAlpha, isAlphaNum, isDigit, ord)
import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map
parse = readP_to_S jValue

data JValue = JObject [(String, JValue)] | JArray [JValue] | JString String deriving (Show)

jValue :: ReadP JValue
jValue = jObject +++ jArray +++ jString

jString :: ReadP JValue
jString =
  do
    skipSpaces
    str <- between (char '"') (char '"') (many1 $ satisfy isAlphaNum)
    pure (JString str)

jObject :: ReadP JValue
jObject =
  do
    skipSpaces
    char '{'
    skipSpaces
    (JString key) <- jString
    skipSpaces
    char ':'
    skipSpaces
    value <- jValue
    char '}'
    pure (JObject [(key, value)])

jArray :: ReadP JValue
jArray =
  do
    skipSpaces
    char '['
    skipSpaces
    value <- jValue
    skipSpaces
    char ']'
    pure value

data TAction = LEFT | RIGHT deriving(Enum)

data TTrans = TTrans {
  read :: Char,
  toState :: String,
  write :: Char,
  action :: TAction
}

data TDesc = TDesc {
  name :: String,
  alphabet :: [Char],
  blank :: Char,
  states :: [String],
  initial :: String,
  finals :: [String],
  transitions :: Map.Map String [TTrans]
}

extractValue :: String -> [(String, JValue)] -> Maybe JValue
extractValue _ [] = Nothing
extractValue toFind xs = case xs of
  ((key, value) : rest) -> if key == toFind
    then Just value
    else extractValue toFind rest
  _ -> Nothing

extractName :: [(String, JValue)] -> Maybe String
extractName ms =
  do
    name <- extractValue "name" ms
    case name of
      JString str -> pure str
      _ -> Nothing

-- extractAlphabet :: [(String, JValue)] -> Maybe [Char]
-- extractAlphabet = extractValue "alphabet"