{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- JsonUtils
-}

module Json.JsonUtils
    ( jsonUnbeautify
    , indent
    ) where

-- =====================
-- PARSING FUNCTIONS
-- =====================

jsonUnbeautify :: String -> String
jsonUnbeautify = go True
  where
    go _ [] = []
    go False ('"' : xs) = '"' : go True xs
    go False (x    : xs) = x : go False xs
    go True ('"' : xs)  = '"' : go False xs
    go True (c : xs)
      | c `elem` " \n\t" = go True xs
      | otherwise        = c : go True xs

-- =====================
-- PRINTER FUNCTIONS
-- =====================

indent :: Int -> String
indent n = replicate (n * 4) ' '
