{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- XmlHeader
-}

module Xml.XmlHeader
    ( parseHeader
    , headerToXml
    ) where

import Control.Applicative (Alternative(..), optional)

import Xml.XmlUtils
import CommonUtils
import BasicParser
import Document

-- =====================
-- HEADER PRINTER
-- =====================

-- | Converts a Header to XML format.
-- The function takes a Header and returns an XML string.
headerToXml :: Header -> String
headerToXml (Header elems) =
  concatMap headerContentToXml elems
  ++ indent 1 ++
  "</header>"

-- | Converts a single HeaderContent to XML format.
-- The function takes a HeaderContent and returns an XML string.
headerContentToXml :: HeaderContent -> String
headerContentToXml (Title title) =
  "<header title=\"" ++ title ++ "\">\n"
headerContentToXml (Author author) =
  indent 2 ++ "<author>" ++ author ++ "</author>\n"
headerContentToXml (Date date) =
  indent 2 ++ "<date>" ++ date ++ "</date>\n"

-- =====================
-- HEADER PARSER
-- =====================

-- | Parses a Header from XML format.
-- The function takes a parser and returns a Header.
parseHeader :: Parser Header
parseHeader = do
  title <- parseHeaderTitle
  elems <- parseHeaderContents
  _ <- parseString "</header>"
  if isHeaderValid (title : elems)
    then return (Header (title : elems))
    else empty

-- | Parses a list of HeaderContent from XML format.
-- The function takes a parser and returns a list of HeaderContent.
parseHeaderContents :: Parser [HeaderContent]
parseHeaderContents =
    (do
        element <- parseHeaderContent
        rest <- parseHeaderContents
        return (element : rest))
    <|> pure []

-- | Parses a single HeaderContent from XML format.
-- The function tries to parse different types of HeaderContent in order.
parseHeaderContent :: Parser HeaderContent
parseHeaderContent =
        parseHeaderAuthor
    <|> parseHeaderDate

-- | Parses a Title from XML format.
-- The function takes a parser and returns a Title.
parseHeaderTitle :: Parser HeaderContent
parseHeaderTitle = do
  _ <- parseString "<header title="
  title <- parseQuotedString
  _ <- parseChar '>'
  _ <- optional parseWhiteSpace
  return (Title title)

-- | Parses an Author from XML format.
-- The function takes a parser and returns an Author.
parseHeaderAuthor :: Parser HeaderContent
parseHeaderAuthor = do
  _ <- parseString "<author>"
  author <- parseUntil '<'
  _ <- parseString "</author>"
  _ <- optional parseWhiteSpace
  return (Author author)

-- | Parses a Date from XML format.
-- The function takes a parser and returns a Date.
parseHeaderDate :: Parser HeaderContent
parseHeaderDate = do
  _ <- parseString "<date>"
  date <- parseUntil '<'
  _ <- parseString "</date>"
  _ <- optional parseWhiteSpace
  return (Date date)
