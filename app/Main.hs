{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Data.Char
import System.Environment (getArgs)

data JsonNumber = JsonInt Integer | JsonFloat Double
  deriving (Show, Eq)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber JsonNumber
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

newtype Parser a = Parser
  { runParser :: String -> Either String (a, String)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, input') <- p input
    Right (f x, input')

instance Applicative Parser where
  pure x = Parser $ \input -> Right (x, input)
  (Parser pf) <*> (Parser px) = Parser $ \input -> do
    (f, input') <- pf input
    (x, input'') <- px input'
    Right (f x, input'')

instance Alternative Parser where
  empty = Parser $ \_ -> Left "Error"
  (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
    Left _ -> p2 input
    Right r -> Right r

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonFloat <|> jsonInt <|> jsonString <|> jsonArray <|> jsonObject

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where
    f "true" = JsonBool True
    f "false" = JsonBool False
    f _ = error "Unreachable"

jsonInt :: Parser JsonValue
jsonInt = JsonNumber . JsonInt . read <$> (positive <|> negative)
  where
    positive = intLiteral
    negative = (:) <$> charP '-' <*> intLiteral

jsonFloat :: Parser JsonValue
jsonFloat = JsonNumber . JsonFloat . read <$> (positive <|> negative)
  where
    positive = (++) <$> intLiteral <*> ((:) <$> charP '.' <*> intLiteral)
    negative = (:) <$> charP '-' <*> positive

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where
    elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> ws *> fields <* ws <* charP '}')
  where
    fields = sepBy (ws *> charP ',' <* ws) pairs
    pairs = (\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> charP ':' <* ws) <*> jsonValue

charP :: Char -> Parser Char
charP x = Parser $ \case
  (c : cs) | c == x -> Right (c, cs)
  (c : _) -> Left $ "Unexpected char " ++ [c]
  _ -> Left "Unexpected EOF"

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> Right (span f input)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (xs, input') <- p input
  if null xs then Left "Empty sequence" else Right (xs, input')

ws :: Parser String
ws = spanP isSpace

stringLiteral :: Parser String
stringLiteral = charP '"' *> many stringChar <* charP '"'

intLiteral :: Parser String
intLiteral = notNull . spanP $ isDigit

stringChar :: Parser Char
stringChar = escaped <|> normal
  where
    normal = Parser $ \case
      ('"' : _) -> Left "End of string"
      ('\\' : _) -> Left "Start of escape"
      (c : cs) -> Right (c, cs)
      [] -> Left "Unexpected EOF"
    escaped = charP '\\' *> (decode <$> anyChar)
    decode 'n' = '\n'
    decode 't' = '\t'
    decode 'r' = '\r'
    decode 'b' = '\b'
    decode 'f' = '\f'
    decode c = c

anyChar :: Parser Char
anyChar = Parser $ \case
  (c : cs) -> Right (c, cs)
  [] -> Left "Unexpected EOF"

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

formatJson :: JsonValue -> Int -> String
formatJson JsonNull _ = "null"
formatJson (JsonBool False) _ = "false"
formatJson (JsonBool True) _ = "true"
formatJson (JsonNumber (JsonInt n)) _ = show n
formatJson (JsonNumber (JsonFloat f)) _ = show f
formatJson (JsonString s) _ = '"' : format s ++ ['"']
  where
    format = concatMap f
    f '"' = "\\\""
    f '\n' = "\\n"
    f '\t' = "\\t"
    f '\b' = "\\b"
    f '\r' = "\\r"
    f '\f' = "\\f"
    f c = [c]
formatJson (JsonArray []) _ = "[]"
formatJson (JsonArray a) n = "[\n" ++ elements ++ replicate n ' ' ++ "]"
  where
    elements = unlines $ f a
    f [x] = [line x]
    f (x : xs) = (line x ++ ",") : f xs
    f [] = error "Unreachable"
    line x = replicate (n + 2) ' ' ++ formatJson x (n + 2)
formatJson (JsonObject []) _ = "{}"
formatJson (JsonObject o) n = "{\n" ++ elements ++ replicate n ' ' ++ "}"
  where
    elements = unlines $ f o
    f [(key, value)] = [line key value]
    f ((key, value) : xs) = (line key value ++ ",") : f xs
    f [] = error "Unreachable"
    line key value = replicate (n + 2) ' ' ++ ['"'] ++ key ++ ['"'] ++ ": " ++ formatJson value (n + 2)

formatString :: String -> String
formatString s = case runParser jsonValue s of
  Right (r, _) -> formatJson r 0
  Left l -> l

formatFile :: FilePath -> IO String
formatFile f = do
  input <- readFile f
  return (formatString input)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] ->
      ( do
          s <- formatFile file
          putStrLn s
      )
    _ -> putStrLn "Usage: json-formatter <file>"
