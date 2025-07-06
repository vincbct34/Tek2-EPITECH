{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- MarkdownUtils
-}

module Markdown.MarkdownUtils
    ( removeTrailingNewlines
    , normalizeLineEndings
    ) where

-- =======================
-- PARSER FUNCTIONS
-- =======================

normalizeLineEndings :: String -> String
normalizeLineEndings [] = []
normalizeLineEndings ('\r':'\n':xs) = '\n' : normalizeLineEndings xs
normalizeLineEndings (x:xs)        = x : normalizeLineEndings xs

-- ======================
-- PRINTER FUNCTIONS
-- ======================

removeTrailingNewlines :: String -> String
removeTrailingNewlines = reverse . dropWhile (== '\n') . reverse
