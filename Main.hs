module Main where

import Parser
import System.Environment

parseFile :: Parser a -> FilePath -> IO (Maybe a)
parseFile parser file = do
  input <- readFile file
  return (snd <$> runParser parser input)

printf :: (Show a) => a -> IO ()
printf content = do
  putStrLn "----------------------------------------"
  print content

main :: IO ()
main = do
  args <- getArgs
  maybeJsonValues <- mapM (parseFile jsonParser) args
  case sequence maybeJsonValues of
    Just jsonValues -> mapM_ printf jsonValues
    Nothing -> print "Error while parsing"
