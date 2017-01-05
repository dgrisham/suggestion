module AddMovie.Parser where
    
-- Imports
-- =======

import Text.Megaparsec
import Text.Megaparsec.String

-- Local
-- -----

import AddMovie.Types


-- Parser
-- ======

p_metadata :: Parser Metadata
p_metadata = Metadata <$> p_query

p_query :: Parser [(Field, Value)]
p_query = p_braces p_lines <* newline

p_lines :: Parser [(Field, Value)]
p_lines = sepBy1 p_line newline

p_line :: Parser (Field, Value)
p_line = do
    field <- p_field <* char ':'
    value <- p_value
    return $ (field, value)

p_field :: Parser Field
p_field = p_quotes $ some p_fieldChar

p_fieldChar :: Parser Char
p_fieldChar = oneOf $ ['A'..'Z'] ++ ['a'..'z']

p_value :: Parser Value
p_value = p_quotes $ some p_valueChar

p_valueChar :: Parser Char
p_valueChar = oneOf $  ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
                    ++ " .,-_/&():@{}$'\\"

p_braces :: Parser a -> Parser a
p_braces = between (char '{') (char '}')

p_quotes :: Parser a -> Parser a
p_quotes = between (char '"') (char '"')

