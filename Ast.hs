module Ast where

import Data.Map qualified as M

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject (M.Map String JsonValue)
  deriving (Show)
