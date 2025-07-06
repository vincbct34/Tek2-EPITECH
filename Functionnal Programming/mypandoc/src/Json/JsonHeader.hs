{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- JsonHeader
-}

module Json.JsonHeader
    ( headerToJson
    , parseHeader
    ) where

import Control.Applicative(Alternative(..))

import Json.JsonUtils
import CommonUtils
import BasicParser
import Document

-- =====================
-- HEADER PRINTER
-- =====================

-- | Converts a header to JSON format.
-- The function takes a Header and returns a JSON string.
headerToJson :: Header -> String
headerToJson (Header elems) =
    "\"header\": {\n" ++
    headerContentToJson elems ++ "\n" ++ indent 1 ++
    "}"

-- | Converts a list of header contents to JSON format.
-- The function takes a list of HeaderContent and returns a JSON string.
headerContentToJson :: [HeaderContent] -> String
headerContentToJson = go
  where
    go [] = ""
    go [e] = headerElemToJson e
    go (e:es) = headerElemToJson e ++ ",\n" ++ go es

-- | Converts a single header content element to JSON format.
-- The function takes a HeaderContent and returns a JSON string.
headerElemToJson :: HeaderContent -> String
headerElemToJson (Title t) = indent 2 ++ "\"title\": \"" ++ t ++ "\""
headerElemToJson (Author a) = indent 2 ++ "\"author\": \"" ++ a ++ "\""
headerElemToJson (Date d) = indent 2 ++ "\"date\": \"" ++ d ++ "\""

-- =====================
-- HEADER PARSER
-- =====================

-- | Parses the header of a document from JSON format.
-- The function takes a parser and returns a Header.
parseHeader :: Parser Header
parseHeader = do
  _ <- parseString "\"header\":{"
  elems <- parseHeaderContents '}'
  _ <- parseChar '}'
  if isHeaderValid elems
    then return (Header elems)
    else empty

-- | Parses a list of header contents from JSON format.
-- The function takes an end character and returns a list of HeaderContent.
parseHeaderContents :: Char -> Parser [HeaderContent]
parseHeaderContents endChar = do
  element <- parseHeaderContent
  next <- (parseChar ',' >> pure ',') <|> pure endChar
  case next of
    ',' -> do
      rest <- parseHeaderContents endChar
      return (element : rest)
    _ -> return [element]

-- | Parses a single header content from JSON format.
-- The function returns a HeaderContent.
parseHeaderContent :: Parser HeaderContent
parseHeaderContent =
      parseHeaderTitle
  <|> parseHeaderAuthor
  <|> parseHeaderDate

-- | Parses the title from JSON format.
-- The function returns a HeaderContent with the title.
parseHeaderTitle :: Parser HeaderContent
parseHeaderTitle = do
  _ <- parseString "\"title\":"
  Title <$> parseQuotedString

-- | Parses the author from JSON format.
-- The function returns a HeaderContent with the author.
parseHeaderAuthor :: Parser HeaderContent
parseHeaderAuthor = do
  _ <- parseString "\"author\":"
  Author <$> parseQuotedString

-- | Parses the date from JSON format.
-- The function returns a HeaderContent with the date.
parseHeaderDate :: Parser HeaderContent
parseHeaderDate = do
  _ <- parseString "\"date\":"
  Date <$> parseQuotedString
