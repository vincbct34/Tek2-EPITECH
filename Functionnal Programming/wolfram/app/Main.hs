{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- Main.hs
-}

module Main (
    main,
    usageMessage
) where

-- Imports from standard libraries
import System.Environment   (getArgs)
import System.Exit          (exitWith, ExitCode(ExitFailure))

-- Imports from local modules
import Automaton            (runAutomaton)
import Parser               (parseArgs)

-- Main function
main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Right config    -> runAutomaton config
        Left errMsg     -> putStrLn errMsg
                        >> putStrLn usageMessage
                        >> exitWith (ExitFailure 84)

-- Usage message
usageMessage :: String
usageMessage = 
    "Usage: ./wolfram --rule RULE [--start START] [--lines LINES] " ++
    "[--window WIDTH] [--move MOVE]"
