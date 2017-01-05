module AddMovie.Types where

-- Imports
-- =======

import Data.List (lookup)

-- Types
-- =====

data Metadata = Metadata [(Field, Value)]
    deriving (Show)

lookupValue :: Field -> Metadata -> Value
lookupValue query (Metadata pairs) =
    case lookup query pairs of
        Just value -> value
        Nothing    -> ""

type Field = String
type Value = String

