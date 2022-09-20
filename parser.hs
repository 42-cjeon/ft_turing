{-# LANGUAGE LambdaCase #-}

import Data.Char (isAlpha, isAlphaNum, isDigit, ord)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.ReadP

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

data TAction = LEFT | RIGHT deriving (Enum)

data TTrans = TTrans
  { toState :: String,
    write :: Char,
    action :: TAction
  }

data TDesc = TDesc
  { name :: String,
    alphabet :: [Char],
    blank :: Char,
    states :: [String],
    initial :: String,
    finals :: [String],
    transitions :: Map.Map String (Map.Map String TTrans)
  }

searchValue :: String -> [(String, JValue)] -> Maybe JValue
searchValue _ [] = Nothing
searchValue toFind xs = case xs of
  ((key, value) : rest) ->
    if key == toFind
      then Just value
      else searchValue toFind rest
  _ -> Nothing

extractString :: JValue -> Maybe String
extractString = \case
  JString x -> Just x
  _ -> Nothing

extractArray :: JValue -> Maybe [JValue]
extractArray = \case
  JArray x -> Just x
  _ -> Nothing

searchString :: String -> [(String, JValue)] -> Maybe String
searchString key xs = searchValue key xs >>= extractString

searchArray :: String -> [(String, JValue)] -> Maybe [JValue]
searchArray key xs = searchValue key xs >>= \x -> extractArray x

jValueToTDesc :: [(String, JValue)] -> Maybe TDesc
jValueToTDesc jvalue =
  do
    name <- searchString "name" jvalue
    alphabet <- searchArray "alphabet" jvalue
    blank <- searchString "blank" jvalue
    states <- searchString "states" jvalue
    initial <- searchString "initial" jvalue
    finals <- extractString <$> searchArray "finals" jvalue
    transitions <- Map.fromList (zip states (jValueToTransition jvalue <$> states))
    pure TDesc {
      name=name,
      alphabet=alphabet,
      blank=blank,
      states=states,
      initial=initial,
      finals=finals,
      transitions=transitions
    }

stringToTAction :: String -> Maybe TAction
stringToTAction str
  | str == "LEFT" = Just LEFT
  | str == "RIGHT" = Just LEFT
  | otherwise = Nothing

jValueToTTrans :: JValue -> Maybe TTrans
jValueToTTrans jvalue =
  do
    read <- searchString "read" jvalue
    toState <- searchString "to_state" jvalue
    write <- searchString "write" jvalue
    action <- searchString "action" >>= \x -> stringToTAction x
    pure (read, TTrans {
      toState=toState,
      write=write,
      action=action
    })

jValueToTransition :: JValue -> String -> Map.Map String TTrans
jValueToTransition jvalue key = Map.fromList (jValueToTTrans <$> searchArray key jvalue)

-- (state, tape) ==> 
-- 



-- parseIntoTDesc :: String -> Maybe TDesc
-- parseIntoTDesc xs = 
--   case json ofg
--     [(v,"")] -> 
--     _ -> Nothing
--   where
--     json = parse jValue xs
-- -- extractString :: String -> [(String, JValue)] -> Maybe String
-- -- extractString find xs = extractValue find xs >>= \case
-- --   JString x -> Just x
-- --   _ -> Nothing
