{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- MarkdownParser
-}

module Markdown.MarkdownParser
    ( markdownToDocument
    ) where

import Markdown.MarkdownHeader
import Markdown.MarkdownBody
import BasicParser
import Document

-- =====================
-- MAIN PARSER
-- =====================

markdownToDocument :: Parser Document
markdownToDocument = do
  header <- parseHeader
  Document header <$> parseBody
