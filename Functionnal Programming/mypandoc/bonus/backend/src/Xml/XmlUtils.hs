{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- XmlUtils
-}

module Xml.XmlUtils
    ( indent
    ) where

-- ======================
-- PRINTER FUNCTIONS
-- ======================

indent :: Int -> String
indent n = replicate (n * 4) ' '
