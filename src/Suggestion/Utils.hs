module Suggestion.Utils where

-- Imports
-- =======

-- # Avoiding a name conflict...
import Prelude hiding (filter)
import qualified Prelude as P (filter)

import System.Random (RandomGen, randomR)
import Data.Char (toLower)
import Data.List.Split (splitOn)

-- Local
-- -----

import Suggestion.Types
import AddMovie.Main (addMovie, exportFields)


-- Movie Selection
-- ===============

-- Movies
-- ------

makeSuggestion :: RandomGen g => Filters -> Movies -> g -> (Title, g)
makeSuggestion filters movies randomGen =
    chooseRandom filteredMovies randomGen
    where
        filteredMovies = filterMovies filters movies

chooseRandom :: RandomGen g => Movies -> g -> (Title, g)
chooseRandom movies randomGen =
    (getTitle $ movies !! randomIndex, randomGen')
    where
        (randomIndex, randomGen') = randomR (0, length movies - 1) randomGen

filterMovies :: Filters -> Movies -> Movies
filterMovies filters = P.filter (movieMatchesAny filters)

movieMatchesAny :: Filters -> Movie -> Bool
movieMatchesAny filters (Movie _ metadata) =
    foldr (||) False . map (metadataMatches metadata) $ filters

metadataMatches :: Metadata -> Filter -> Bool
metadataMatches (Metadata metadataMap) filter =
    foldr (&&) True . map (metadataMapMatches metadataMap) $ filter

metadataMapMatches :: MetadataMap -> SubFilter -> Bool
metadataMapMatches metadataMap (SubFilter field value) =
    case lookup field metadataMap of
        Just metadataValue -> all (`elem` metadataValue) value
        Nothing            -> True

-- Filters
-- -------

parseFilters :: [String] -> [Filter]
parseFilters = map parseFilter

parseFilter :: String -> Filter
parseFilter = map parseSubFilter . splitFilter

splitFilter :: String -> [String]
splitFilter = splitOn ";"

parseSubFilter :: String -> SubFilter
parseSubFilter filterStr = SubFilter (cleanUp field) values
    where
        values = map cleanUp . splitOn "," $ valueStr
        (field, valueStr) = stripField filterStr

stripField :: String -> (Field, String)
stripField filterStr = (field, valueStr)
    where
        field:valueStr:[] = splitOn ":" filterStr

updateMetadata :: Metadata -> Metadata
updateMetadata metadata = addField metadata "Decade" [decade]
    where
        decade = take 3 year ++ "0s"
        year = lookupValue "Year" metadata !! 0

supportedFields :: [String]
supportedFields = map toLower' $ exportFields ++ [ "Decade" ]

