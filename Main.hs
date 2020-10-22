{-# Language InstanceSigs #-}

module Main where

import Control.Applicative
import Data.Char (isDigit)

---- types:

data JsonValue =
  JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- TODO: Support decimals
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

newtype Parser a = Parser {
  runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser fa) = Parser $ \input -> do
    (input', a) <- fa input
    Just (input', f a)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \input -> Just (input, x)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser fa <*> Parser fb = Parser $ \input -> do
    (input', f)  <- fa input
    (input'', v) <- fb input'
    Just (input'', f v)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser fa <|> Parser fb = Parser $ \input -> fa input <|> fb input

---- helpers:

charParser :: Char -> Parser Char
charParser x = Parser $ \input -> case input of
  (y:ys) -> if y == x then Just (ys, x) else Nothing
  _      -> Nothing

stringParser :: String -> Parser String
stringParser = traverse charParser

spanParser :: (Char -> Bool) -> Parser String
spanParser p = Parser $ \input ->
  case span p input of
    (x:xs, rest) -> Just (rest, x:xs)
    _            -> Nothing

ws :: Parser String
ws = many (charParser ' ')

sepByParser :: Char -> Parser a -> Parser [a]
sepByParser c pa = (:) <$> (ws *> pa <* ws) <*> many (charParser c *> ws *> pa <* ws)

stringLiteralParser :: Parser String
stringLiteralParser = charParser '"' *> spanParser (/= '"') <* charParser '"'

---- components:

jsonNullParser :: Parser JsonValue
jsonNullParser = JsonNull <$ stringParser "null"

jsonBoolParser :: Parser JsonValue
jsonBoolParser = (JsonBool True <$ stringParser "true") <|>
  (JsonBool False <$ stringParser "false")

jsonNumberParser :: Parser JsonValue
jsonNumberParser = JsonNumber . read <$> spanParser isDigit

-- TODO: Support escape sequences
jsonStringParser :: Parser JsonValue
jsonStringParser = JsonString <$>
  (charParser '"' *> spanParser (/= '"') <* charParser '"')

jsonArrayParser :: Parser JsonValue
jsonArrayParser = JsonArray <$> (
  charParser '[' *> ws *>
  sepByParser ',' jsonParser
  <* ws <* charParser ']')

kvPairParser :: Parser (String, JsonValue)
kvPairParser = (\s _ j -> (s, j)) <$>
  stringLiteralParser <*>
  (ws *> charParser ':' <* ws) <*>
  jsonParser

jsonObjectParser :: Parser JsonValue
jsonObjectParser = JsonObject <$> (
  ws *> charParser '{' *>
  sepByParser ',' kvPairParser
  <* charParser '}' <* ws)

---- main:

jsonParser :: Parser JsonValue
jsonParser = jsonNullParser <|> jsonBoolParser <|> jsonNumberParser <|>
  jsonStringParser <|> jsonArrayParser <|> jsonObjectParser

main :: IO ()
main = interact $ \input -> case runParser jsonParser input of
  Just (r, x) -> "Json parser output: " ++ show x ++ "\n" ++ "Remaining: " ++ r
  Nothing -> "Error Parsing input as json"

---- tests: (require dante for emacs)

-- Positive:

-- >>> runParser (charParser 'n') "null"
-- Just ("ull",'n')
-- >>> runParser (stringParser "null") "nullval"
-- Just ("val","null")
-- >>> runParser (jsonNullParser) "nullval"
-- Just ("val",JsonNull)
-- >>> runParser (jsonBoolParser) "falseval"
-- Just ("val",JsonBool False)
-- >>> runParser jsonNumberParser "34sdfk"
-- Just ("sdfk",JsonNumber 34)
-- >>> runParser (sepByParser ',' jsonNumberParser) "23,  45"
-- Just ("",[JsonNumber 23,JsonNumber 45])
-- >>> runParser (ws *> jsonNumberParser <* ws) "   34    "
-- Just ("",JsonNumber 34)
-- >>> runParser jsonParser "[23,   45]"
-- Just ("",JsonArray [JsonNumber 23,JsonNumber 45])
-- >>> runParser stringLiteralParser "\"hello\""
-- Just ("","hello")
-- >>> runParser jsonObjectParser "{  \"key\"  :  45  }"
-- Just ("",JsonObject [("key",JsonNumber 45)])
-- >>> runParser jsonParser "\"hello\""
-- Just ("",JsonString "hello")
-- >>> runParser jsonParser "{  \"key\"  :  45  }"
-- Just ("",JsonObject [("key",JsonNumber 45)])
-- >>> runParser jsonParser "{  \"key1\"  :  [45, \"haskell is awesome!!\"], \"key2\" : null, \"key3\"  : [true, false] };"
-- Just (";",JsonObject [("key1",JsonArray [JsonNumber 45,JsonString "haskell is awesome!!"]),("key2",JsonNull),("key3",JsonArray [JsonBool True,JsonBool False])])

-- Negative:

-- >>> runParser (jsonNullParser) "nulval"
-- Nothing
-- >>> runParser (spanParser isDigit) "sljf234"
-- Nothing
