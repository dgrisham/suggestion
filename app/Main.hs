module Main where

-- Imports
-- =======

import System.Environment (getArgs)

-- Local
-- -----

import Lib (inputHandler)

main :: IO ()
main = do
    inputArgs <- getArgs
    inputHandler inputArgs
