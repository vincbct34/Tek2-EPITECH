{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- MarkdownHeader
-}

module Markdown.MarkdownHeader
    ( parseHeader
    , headerToMarkdown
    ) where

import Control.Applicative (Alternative(..))

import BasicParser
import CommonUtils
import Document

-- =====================
-- HEADER PRINTER
-- =====================

-- | Converts a Header to Markdown format.
-- The function takes a Header and returns a Markdown string.
headerToMarkdown :: Header -> String
headerToMarkdown (Header elems) =
    "---\n" ++
    headerContentToMarkdown elems ++
    "---\n"

-- | Converts a list of HeaderContent to Markdown format.
-- The function takes a list of HeaderContent and returns a Markdown string.
headerContentToMarkdown :: [HeaderContent] -> String
headerContentToMarkdown = go
  where
    go [] = ""
    go [e] = headerElemToMarkdown e
    go (e:es) = headerElemToMarkdown e ++ go es

-- | Converts a single HeaderContent to Markdown format.
-- The function takes a HeaderContent and returns a Markdown string.
headerElemToMarkdown :: HeaderContent -> String
headerElemToMarkdown (Title title) = "title: " ++ title ++ "\n"
headerElemToMarkdown (Author author) = "author: " ++ author ++ "\n"
headerElemToMarkdown (Date date) = "date: " ++ date ++ "\n"

-- =====================
-- HEADER PARSER
-- =====================

-- | Parses a Header from Markdown format.
-- The function takes a parser and returns a Header.
parseHeader :: Parser Header
parseHeader = do
  _ <- parseString "---\n"
  elems <- parseHeaderContents
  if isHeaderValid elems
    then return (Header elems)
    else empty

-- | Parses a list of HeaderContent from Markdown format.
-- The function takes a parser and returns a list of HeaderContent.
parseHeaderContents :: Parser [HeaderContent]
parseHeaderContents = do
  end <- (parseString "---\n" >> pure True) <|> pure False
  if end
    then return []
    else do
      element <- parseHeaderContent
      _ <- parseChar '\n'
      rest <- parseHeaderContents
      return (element : rest)

-- | Parses a single HeaderContent from Markdown format.
-- The function tries to parse different types of HeaderContent in order.
parseHeaderContent :: Parser HeaderContent
parseHeaderContent =
      parseTitle
  <|> parseAuthor
  <|> parseDate

-- | Parses a Title from Markdown format.
-- The function takes a parser and returns a Title.
parseTitle :: Parser HeaderContent
parseTitle = do
  _ <- parseString "title: "
  title <- parseUntil '\n'
  return (Title title)

-- | Parses an Author from Markdown format.
-- The function takes a parser and returns an Author.
parseAuthor :: Parser HeaderContent
parseAuthor = do
  _ <- parseString "author: "
  author <- parseUntil '\n'
  return (Author author)

-- | Parses a Date from Markdown format.
-- The function takes a parser and returns a Date.
parseDate :: Parser HeaderContent
parseDate = do
  _ <- parseString "date: "
  date <- parseUntil '\n'
  return (Date date)