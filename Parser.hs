module Parser where

import Ast
import Control.Applicative
import Data.Char
import Data.Map qualified as M

-- TODO: support proper error reports
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser p'
    where
      p' input = do
        (input', x) <- p input
        return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  Parser p1 <*> Parser p2 = Parser p3
    where
      p3 input = do
        (input', f) <- p1 input
        (input'', x) <- p2 input'
        return (input'', f x)

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 =
    Parser $ \input -> p1 input <|> p2 input

makeCharParser :: Char -> Parser Char
makeCharParser x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

makeStrParser :: String -> Parser String
makeStrParser = traverse makeCharParser

spanParser :: (Char -> Bool) -> Parser String
spanParser pred = Parser p
  where
    p input =
      let (digits, rest) = span pred input
       in return (rest, digits)

guardNull :: Parser [a] -> Parser [a]
guardNull (Parser p) = Parser p'
  where
    p' input = do
      (input', x) <- p input
      if null x then Nothing else Just (input', x)

-- TODO: support escaping
stringLiteralParser :: Parser String
stringLiteralParser = makeCharParser '"' *> spanParser (/= '"') <* makeCharParser '"'

whiteSpaceParser :: Parser String
whiteSpaceParser = spanParser isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sepP elementP = (:) <$> elementP <*> many (sepP *> elementP) <|> pure []

jsonNullParser :: Parser JsonValue
jsonNullParser = JsonNull <$ makeStrParser "null"

jsonBoolParser :: Parser JsonValue
jsonBoolParser = f <$> (makeStrParser "true" <|> makeStrParser "false")
  where
    f "true" = JsonBool True
    f "false" = JsonBool False
    f _ = error "Unreachable: only true | false can be parsed in here"

jsonNumberParser :: Parser JsonValue
jsonNumberParser = f <$> guardNull (spanParser isDigit)
  where
    f digits = JsonNumber (read digits)

jsonStringParser :: Parser JsonValue
jsonStringParser = JsonString <$> stringLiteralParser

jsonArrayParser :: Parser JsonValue
jsonArrayParser =
  JsonArray
    <$> ( makeCharParser '['
            *> whiteSpaceParser
            *> elementsParser
            <* whiteSpaceParser
            <* makeCharParser ']'
        )
  where
    sep = whiteSpaceParser *> makeCharParser ',' <* whiteSpaceParser
    elementsParser = sepBy sep jsonParser

jsonObjectParser :: Parser JsonValue
jsonObjectParser =
  JsonObject . M.fromList
    <$> ( makeCharParser '{'
            *> whiteSpaceParser
            *> pairsParser
            <* whiteSpaceParser
            <* makeCharParser '}'
        )
  where
    pairParser =
      (\key _ value -> (key, value))
        <$> stringLiteralParser
        <*> (whiteSpaceParser *> makeCharParser ':' <* whiteSpaceParser)
        <*> jsonParser
    pairsParser =
      sepBy (whiteSpaceParser *> makeCharParser ',' <* whiteSpaceParser) pairParser

jsonParser :: Parser JsonValue
jsonParser =
  jsonNullParser
    <|> jsonBoolParser
    <|> jsonNumberParser
    <|> jsonStringParser
    <|> jsonArrayParser
    <|> jsonObjectParser
