{-
-- EPITECH PROJECT, 2025
-- ImageCompressor
-- File description:
-- Main.hs
-}

module Main (main) where

import Utilities (usageMessage, putError)
import Compressor (mainProcess)

import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Right (n, l, file) -> mainProcess n l file
        Left errMsg        -> putError errMsg

parseArgs :: [String] -> Either String (Int, Float, FilePath)
parseArgs ["-n", nStr, "-l", lStr, "-f", file] =
    case (readMaybe nStr :: Maybe Int,
          readMaybe lStr :: Maybe Float) of
        (Just n, Just l)
            | n > 0 && l > 0 -> Right (n, l, file)
            | otherwise      -> Left "Error: -n and -l must be > 0"
        _ -> Left "Error: -n and -l must be valid numbers"
parseArgs [] = Left usageMessage
parseArgs _  = Left "Error: Invalid arguments"
