module Suggestion.Parser where
    
-- Imports
-- =======

import Data.Char (toLower)
import Data.List.Split (splitOn)
import Text.Megaparsec
import Text.Megaparsec.String

-- Local
-- -----

import Suggestion.Types
import AddMovie.Parser (p_fieldChar, p_valueChar)


-- Parsers
-- =======

p_movies :: Parser Movies
p_movies = many p_movie

p_movie :: Parser Movie
p_movie = do
    title    <- p_title <* newline
    metadata <- p_metadata
    return $ Movie title metadata

p_title :: Parser Title
p_title = string "Title: " *> (trim <$> some p_valueChar)

p_metadata :: Parser Metadata
p_metadata = Metadata <$> (p_metadata' <* newline)
    where
        p_metadata' = endBy1 p_line newline

p_line :: Parser (Field, Value)
p_line = do
    field <- p_field <* string ": "
    value <- p_value
    return $ (toLower' field, map toLower' value)

p_field :: Parser Field
p_field = some p_fieldChar

p_value :: Parser Value
p_value = stringToValue <$> some p_valueChar

