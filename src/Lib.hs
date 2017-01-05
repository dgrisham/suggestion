module Lib
    ( inputHandler
    ) where

-- Imports
-- =======

import System.Random (getStdGen)
import Text.Megaparsec (runParser, parseErrorPretty)

-- Local
-- -----

import Suggestion.Parser (p_movies)
import Suggestion.Utils
import AddMovie.Main (addMovie)


-- User Input
-- ==========

inputHandler :: [String] -> IO ()
inputHandler ("add":title:[]) = addMovie title
inputHandler ("fields":[]) = do
    putStr "Available filters:\n"
    putStr . unlines $ supportedFields
    return ()
inputHandler input = do
    let inputFilters = parseFilters input
    parseResult <- parseFromFile p_movies movieFile
    randomGen <- getStdGen
    case parseResult of
        Left error -> print $ parseErrorPretty error
        Right movies -> print . fst $
            makeSuggestion inputFilters movies randomGen

-- Helper functions/values
-- -----------------------

movieFile :: FilePath
movieFile = "/home/grish/personal/suggestions/movies"

parseFromFile p file = runParser p file <$> readFile file

