{-# LANGUAGE LambdaCase #-}

module Parser where

import Data.Char (isAlpha, isAlphaNum, isDigit, ord, isPrint)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.ReadP
import Data.List

unwrap s = case parsed of
  [(JObject json, "")] -> json
  where parsed = parse s

parse = readP_to_S jValue

data JValue = JObject [(String, JValue)] | JArray [JValue] | JString String deriving (Show)

jValue :: ReadP JValue
jValue = jObject +++ jArray +++ jString

jString :: ReadP JValue
jString =
  do
    skipSpaces
    str <- between (char '"') (char '"') (many1 $ satisfy (\x -> isPrint x && (x /= '"')))
    pure (JString str)

jKeyValue :: ReadP (String, JValue)
jKeyValue =
  do
    skipSpaces
    (JString key) <- jString
    skipSpaces
    char ':'
    skipSpaces
    value <- jValue
    skipSpaces
    pure (key, value)

jObject :: ReadP JValue
jObject =
  do
    skipSpaces
    char '{'
    skipSpaces
    kv <- sepBy jKeyValue (char ',')
    skipSpaces
    char '}'
    pure (JObject kv)

jArray :: ReadP JValue
jArray =
  do
    skipSpaces
    char '['
    skipSpaces
    value <- sepBy jValue $ char ','
    skipSpaces
    char ']'
    pure (JArray value)

data TAction = LEFT | RIGHT deriving (Enum, Show)

data TTrans = TTrans
  { toState :: String,
    write :: String,
    action :: TAction
  } deriving (Show)

data TDesc = TDesc
  { name :: String,
    alphabet :: [String],
    blank :: String,
    states :: [String],
    initial :: String,
    finals :: [String],
    transitions :: Map.Map String (Map.Map String TTrans)
  } deriving (Show)

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

combine :: [Maybe a] -> Maybe [a]
combine [] = Just []
combine (x : xs) = case x of
  Nothing -> Nothing
  Just x' -> case combine xs of
    Nothing -> Nothing
    Just xs' -> Just (x' : xs')

searchString :: String -> [(String, JValue)] -> Maybe String
searchString key xs = searchValue key xs >>= extractString

searchArray :: String -> [(String, JValue)] -> Maybe [JValue]
searchArray key xs = searchValue key xs >>= \x -> extractArray x

jValueToTDesc :: [(String, JValue)] -> Maybe TDesc
jValueToTDesc jvalue =
  do
    name <- searchString "name" jvalue
    alphabet <- searchArray "alphabet" jvalue
    alphabet' <- combine $ extractString <$> alphabet
    blank <- searchString "blank" jvalue
    states <- searchArray "states" jvalue
    states' <- combine $ extractString <$> states
    initial <- searchString "initial" jvalue
    finals <- searchArray "finals" jvalue
    finals' <- combine $ extractString <$> finals
    transitions <- searchValue "transitions" jvalue
    transitions' <- combine $ jValueToTransition transitions <$> (states' \\ finals')
    transitions'' <- Just $ Map.fromList $ zip states' transitions'
    pure TDesc {
      name=name,
      alphabet=alphabet',
      blank=blank,
      states=states',
      initial=initial,
      finals=finals',
      transitions=transitions''
    }

stringToTAction :: String -> Maybe TAction
stringToTAction str
  | str == "LEFT" = Just LEFT
  | str == "RIGHT" = Just RIGHT
  | otherwise = Nothing

jValueToTTrans :: JValue -> Maybe (String, TTrans)
jValueToTTrans (JObject jvalue) =
  do
    read <- searchString "read" jvalue
    toState <- searchString "to_state" jvalue
    write <- searchString "write" jvalue
    action <- searchString "action" jvalue
    action' <- stringToTAction action
    pure (read, TTrans {
      toState=toState,
      write=write,
      action=action'
    })

jValueToTTrans _ = Nothing

jValueToTransition :: JValue -> String -> Maybe (Map.Map String TTrans)
jValueToTransition (JObject jvalue) key =
  do
    arr <- searchArray key jvalue
    transition <- combine $ jValueToTTrans <$> arr
    pure $ Map.fromList transition

jValueToTransition _ _ = Nothing
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