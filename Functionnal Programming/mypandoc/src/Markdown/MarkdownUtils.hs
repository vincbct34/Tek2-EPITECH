{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- MarkdownUtils
-}

module Markdown.MarkdownUtils
    ( removeTrailingNewlines
    , markdownUnbeautify
    ) where

-- =======================
-- PARSER FUNCTIONS
-- =======================

markdownUnbeautify :: String -> String
markdownUnbeautify ('\n':'\n':xs) = '\n' : markdownUnbeautify xs
markdownUnbeautify (x:xs)         = x : markdownUnbeautify xs
markdownUnbeautify []             = []

-- ======================
-- PRINTER FUNCTIONS
-- ======================

removeTrailingNewlines :: String -> String
removeTrailingNewlines = reverse . dropWhile (== '\n') . reverse
