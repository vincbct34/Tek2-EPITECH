{-
-- EPITECH PROJECT, 2025
-- ImageCompressor
-- File description:
-- Compressor.hs
-}

module Compressor (
    mainProcess
) where

import Parser (globalParsing)
import Display (displayLogic)

import Control.Exception (try, IOException)
import System.Exit (exitWith, ExitCode(..))

-- Point d'entrée de l'algorithme
mainProcess :: Int -> Float -> FilePath -> IO ()
mainProcess n limit filePath = do
    result <- try (readFile filePath) :: IO (Either IOException String)
    case result of
        Left _        -> putStrLn "Error: Cannot read input file"
                         >> exitWith (ExitFailure 84)
        Right content ->
            case globalParsing content of
                Left err     -> putStrLn err >> exitWith (ExitFailure 84)
                Right pixels -> displayLogic n limit pixels
