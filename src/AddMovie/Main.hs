module AddMovie.Main
    ( addMovie
    , exportFields
    ) where
    
-- Imports
-- =======

import System.Environment (getArgs)
import System.Process (readProcess)
import Text.Megaparsec (runParser, parseErrorPretty)

-- Local
-- -----

import AddMovie.Types
import AddMovie.Parser (p_metadata)


-- Main
-- ====

addMovie movieTitle = do
    queryResult <- movieQuery movieTitle
    let parseResult = parseMetadata queryResult
    case parseResult of
        Left error -> putStr (parseErrorPretty error)
        Right metadata -> updateMovies metadata
    return ()

updateMovies :: Metadata -> IO ()
updateMovies metadata = appendFile moviesFile (metadataToStr metadata)

metadataToStr :: Metadata -> String
metadataToStr metadata = concatMap (fieldToStr metadata) exportFields ++ "\n"

fieldToStr :: Metadata -> Field -> String
fieldToStr metadata field =
    field ++ ": " ++ (lookupValue field metadata) ++ "\n"

exportFields :: [String]
exportFields = [ "Title"
                  , "Year"
                  , "Genre"
                  , "Director"
                  , "Actors"
                  , "Runtime"
                  , "Rated"
                  , "imdbRating"
                  ]

parseMetadata = runParser p_metadata ""
--parseFromFile p file = runParser p file <$> readFile file

movieQuery :: String -> IO String
movieQuery title =
    readProcess queryTool ["-t", title, "--format", "markdown"] ""

moviesFile :: FilePath
moviesFile = "/home/grish/personal/suggestions/movies"

queryTool :: String
queryTool = "/home/grish/src/suggestion/bin/imdbtool"

filePrefix :: String
filePrefix = "/home/grish/personal/suggestions/"

