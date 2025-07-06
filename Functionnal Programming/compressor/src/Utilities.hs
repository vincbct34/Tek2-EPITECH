{-
-- EPITECH PROJECT, 2025
-- ImageCompressor
-- File description:
-- Utilities.hs
-}

module Utilities (
    Color, Pixel, Point, Cluster,
    usageMessage, isPositiveNumber, putError, splitOnComma
) where

import System.Exit (exitWith, ExitCode(..))

type Color = (Int, Int, Int)
type Point = (Int, Int)
type Pixel = (Point, Color)
type Cluster = [Pixel]

-- Message d'usage
usageMessage :: String
usageMessage = unlines [
        "USAGE: ./imageCompressor -n N -l L -f F\n",
        "   N   number of colors in the final image",
        "   L   convergence limit",
        "   F   path to the file containing the colors of the pixels"
    ]

-- Vérifie si une chaîne de caractères est un nombre positif
isPositiveNumber :: String -> Bool
isPositiveNumber nbString = (read nbString :: Float) > 0

-- Ecrit un message d'erreur et quitte le programme
putError :: String -> IO ()
putError msg = putStrLn msg >> exitWith (ExitFailure 84)

-- Split une chaîne de caractères en fonction d'une virgule
split :: Eq a => a -> [a] -> [[a]]
split _ []    = []
split delim s =
    let (before, rest) = span (/= delim) s
    in before : case rest of
        [] -> []
        (_:after) -> split delim after

-- Split une chaîne de caractères en fonction d'une virgule
splitOnComma :: String -> [String]
splitOnComma = split ','
