{-
-- EPITECH PROJECT, 2025
-- ImageCompressor
-- File description:
-- Parser.hs
-}

module Parser (globalParsing) where

import Utilities (Pixel, Point, Color, Cluster, splitOnComma)

-- Parsing global du contenu du fichier
globalParsing :: String -> Either String Cluster
globalParsing content = Right $ map parseLine (lines content)

-- Parse une ligne de type "(x,y) (r,g,b)"
parseLine :: String -> Pixel
parseLine str =
    case words str of
        [ptStr, colorStr] -> (parsePoint ptStr, parseColor colorStr)
        _ -> error "Invalid line format"

-- Parse un point "(x,y)" → (x, y)
parsePoint :: String -> Point
parsePoint s =
    let trimmed = init (drop 1 s)
    in case splitOnComma trimmed of
        [xStr, yStr] -> (read xStr, read yStr)
        _ -> error "Invalid point format"

-- Parse une couleur "(r,g,b)" → (r, g, b)
parseColor :: String -> Color
parseColor s =
    let trimmed = init (drop 1 s)
    in case splitOnComma trimmed of
        [rStr, gStr, bStr] -> (read rStr, read gStr, read bStr)
        _ -> error "Invalid color format"
