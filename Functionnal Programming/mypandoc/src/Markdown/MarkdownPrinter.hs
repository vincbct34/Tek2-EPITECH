{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- MarkdownPrinter
-}

module Markdown.MarkdownPrinter
    ( documentToMarkdown
    ) where

import Markdown.MarkdownHeader
import Markdown.MarkdownBody
import Document

-- =====================
-- MARKDOWN PRINTER
-- =====================

documentToMarkdown :: Document -> String
documentToMarkdown doc =
    headerToMarkdown (docHeader doc) ++
    bodyToMarkdown (docBody doc)
