module Ast where

import Data.Map.Strict qualified as M

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- TODO: support floats
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject (M.Map String JsonValue)
  deriving (Show, Eq)
