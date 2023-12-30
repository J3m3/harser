module Parser where

import Ast

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

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

strP :: String -> Parser String
strP = traverse charP

jsonNull :: Parser JsonValue
jsonNull = undefined

jsonValue :: Parser JsonValue
jsonValue = undefined
