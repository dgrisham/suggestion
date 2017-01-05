module Suggestion.Types where

-- Imports
-- =======

import Data.Char (toLower)
import Data.List.Split (splitOn)

-- Local
-- -----


-- Types
-- =====

type Movies = [Movie]

data Movie = Movie Title Metadata
    deriving (Show)
type Title = String

type Filters = [Filter]
type Filter = [SubFilter]
data SubFilter = SubFilter Field Value
    deriving (Show)

data Metadata = Metadata MetadataMap
    deriving (Show)
type MetadataMap = [(Field, Value)]

type Field = String
type Value = [String]

-- Functionality
-- -------------

getTitle :: Movie -> Title
getTitle (Movie title _) = title

lookupValue :: Field -> Metadata -> Value
lookupValue query (Metadata pairs) =
    case lookup query pairs of
        Just value -> value
        Nothing    -> []

addField :: Metadata -> Field -> Value -> Metadata
addField (Metadata pairs) field value = Metadata $ (field, value) : pairs

stringToValue :: String -> Value
stringToValue = map cleanUp . splitOn (",")

-- Helper functions
-- ----------------

cleanUp :: String -> String
cleanUp = toLower' . trim

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

isSpace :: Char -> Bool
isSpace = (== ' ')

toLower' :: String -> String
toLower' = map toLower

