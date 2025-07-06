{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- JsonBody
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fromMaybe" #-}

module Json.JsonBody
  ( bodyToJson
  , parseBody
  ) where

import Control.Applicative (Alternative(..), optional)
import Data.List (intercalate)

import Json.JsonUtils
import BasicParser
import Document

-- =====================
-- BODY PRINTER
-- =====================

-- | Converts the body of a document to JSON format.
-- The function takes a list of elements and returns a JSON string.
bodyToJson :: [Element] -> String
bodyToJson elems =
  "\"body\": [\n" ++
  elementsListToJson 2 elems ++ "\n" ++ indent 1 ++
  "]"

-- | Converts a list of elements to JSON format.
-- The function takes an indentation level and a list of elements.
elementsListToJson :: Int -> [Element] -> String
elementsListToJson tab elems =
  let jsonElems = map (elementToJson tab) elems
  in concat $ addCommasBetween jsonElems

-- | Adds commas between elements in a list of strings.
-- The function takes a list of strings and returns a new list with commas.
addCommasBetween :: [String] -> [String]
addCommasBetween []     = []
addCommasBetween [x]    = [x]
addCommasBetween (x:xs) = (x ++ ",\n") : addCommasBetween xs

-- | Converts a single element to JSON format.
-- The function takes an indentation level and an element.
elementToJson :: Int -> Element -> String
elementToJson tab (Text t) =
  indent tab ++ show t
elementToJson tab (Italic t) =
  indent tab ++ "{\n"
  ++ indent (tab + 1) ++ "\"italic\": " ++ show t ++ "\n" ++
  indent tab ++ "}"
elementToJson tab (Bold t) =
  indent tab ++ "{\n"
  ++ indent (tab + 1) ++ "\"bold\": " ++ show t ++ "\n" ++
  indent tab ++ "}"
elementToJson tab (Code t) =
  indent tab ++ "{\n"
  ++ indent (tab + 1) ++ "\"code\": " ++ show t ++ "\n" ++
  indent tab ++ "}"
elementToJson tab (Link url content) =
  indent tab ++ "{\n"
  ++ indent (tab + 1) ++ "\"link\": {\n"
  ++ indent (tab + 2) ++ "\"url\": " ++ show url ++ ",\n"
  ++ indent (tab + 2) ++ "\"content\": [\n"
  ++ elementsListToJson (tab + 3) content ++ "\n"
  ++ indent (tab + 2) ++ "]\n"
  ++ indent (tab + 1) ++ "}\n" ++
  indent tab ++ "}"
elementToJson tab (Image url alt) =
  indent tab ++ "{\n"
  ++ indent (tab + 1) ++ "\"image\": {\n"
  ++ indent (tab + 2) ++ "\"url\": " ++ show url ++ ",\n"
  ++ indent (tab + 2) ++ "\"alt\": [\n"
  ++ elementsListToJson (tab + 3) alt ++ "\n"
  ++ indent (tab + 2) ++ "]\n"
  ++ indent (tab + 1) ++ "}\n" ++
  indent tab ++ "}"
elementToJson tab (Paragraph elems) =
  indent tab ++ "[\n"
  ++ elementsListToJson (tab + 1) elems ++ "\n" ++
  indent tab ++ "]"
elementToJson tab (Section maybeTitle elems) =
  indent tab ++ "{\n"
  ++ indent (tab + 1) ++ "\"section\": {\n"
  ++ indent (tab + 2) ++ "\"title\": "
  ++ show (maybe "" id maybeTitle) ++ ",\n"
  ++ indent (tab + 2) ++ "\"content\": [\n"
  ++ elementsListToJson (tab + 3) elems ++ "\n"
  ++ indent (tab + 2) ++ "]\n"
  ++ indent (tab + 1) ++ "}\n" ++
  indent tab ++ "}"
elementToJson tab (CodeBlock elems) =
  indent tab ++ "{\n"
  ++ indent (tab + 1) ++ "\"codeblock\": [\n"
  ++ elementsListToJson (tab + 2) elems ++ "\n" ++
  indent (tab + 1) ++ "]\n" ++
  indent tab ++ "}"
elementToJson tab (List items) =
  indent tab ++ "{\n" ++
  indent (tab + 1) ++ "\"list\": [\n" ++
  intercalate ",\n" (map (itemToJson (tab + 2)) items) ++ "\n" ++
  indent (tab + 1) ++ "]\n" ++
  indent tab ++ "}"
elementToJson _ _ = error "Unsupported element type for JSON conversion"

-- | Converts a single list item to JSON format.
-- The function takes an indentation level and an element.
itemToJson :: Int -> Element -> String
itemToJson tab (Item elems) =
  elementsListToJson tab elems
itemToJson _ e = error $ "Expected Item, got: " ++ show e

-- ==================
-- BODY PARSER
-- ==================

-- | Parses the body of a document from JSON format.
-- The function takes a parser and returns a list of elements.
parseBody :: Parser [Element]
parseBody = do
  _ <- parseString "\"body\":["
  elems <- parseBodyElements ']' <|> parseEmptyList
  _ <- parseChar ']'
  return elems

-- | Parses a list of elements from JSON format.
-- The function takes an end character and returns a list of elements.
parseBodyElements :: Char -> Parser [Element]
parseBodyElements endChar = do
  element <- parseBodyElement
  next <- (parseChar ',' >> pure ',') <|> pure endChar
  case next of
    ',' -> do
      rest <- parseBodyElements endChar
      return (element : rest)
    _ -> return [element]

-- | Parses an element from JSON format.
-- The function tries to parse different types of elements in order.
parseBodyElement :: Parser Element
parseBodyElement =
      parseBodyText <|> parseBodyItalic
  <|> parseBodyBold <|> parseBodyCode
  <|> parseBodyLink <|> parseBodyImage
  <|> parseBodyParagraph <|> parseBodySection
  <|> parseBodyCodeBlock <|> parseBodyList

-- | Parses a quoted string from JSON format.
-- The function takes a parser and returns a string.
parseBodyText :: Parser Element
parseBodyText = Text <$> parseQuotedString

-- | Parses a simple object from JSON format.
-- The function takes a key and a constructor function.
parseSimpleObject :: String -> (String -> Element) -> Parser Element
parseSimpleObject key constructor = do
  _ <- parseString ("{\"" ++ key ++ "\":")
  content <- parseQuotedString
  _ <- parseChar '}'
  return (constructor content)

-- | Parses an italic object from JSON format.
-- The function takes a parser and returns an italic element.
parseBodyItalic :: Parser Element
parseBodyItalic = parseSimpleObject "italic" Italic

-- | Parses a bold object from JSON format.
-- The function takes a parser and returns a bold element.
parseBodyBold :: Parser Element
parseBodyBold = parseSimpleObject "bold" Bold

-- | Parses a code object from JSON format.
-- The function takes a parser and returns a code element.
parseBodyCode :: Parser Element
parseBodyCode = parseSimpleObject "code" Code

-- | Parses a link object from JSON format.
-- The function takes a parser and returns a link element.
parseBodyLink :: Parser Element
parseBodyLink = do
  _ <- parseString "{\"link\":{\"url\":"
  url <- parseQuotedString
  _ <- parseString ",\"content\":["
  content <- parseBodyElements ']' <|> parseEmptyList
  _ <- parseString "]}}"
  return (Link url content)

-- | Parses an image object from JSON format.
-- The function takes a parser and returns an image element.
parseBodyImage :: Parser Element
parseBodyImage = do
  _ <- parseString "{\"image\":{\"url\":"
  url <- parseQuotedString
  _ <- parseString ",\"alt\":["
  alt <- parseBodyElements ']' <|> parseEmptyList
  _ <- parseString "]}}"
  return (Image url alt)

-- | Parses a paragraph object from JSON format.
-- The function takes a parser and returns a paragraph element.
parseBodyParagraph :: Parser Element
parseBodyParagraph = do
  _ <- parseChar '['
  elems <- parseBodyElements ']' <|> parseEmptyList
  _ <- parseChar ']'
  return (Paragraph elems)

-- | Parses a section object from JSON format.
-- The function takes a parser and returns a section element.
parseBodySection :: Parser Element
parseBodySection = do
  _ <- parseString "{\"section\":{"
  title <- optional
    (parseString "\"title\":" >> parseQuotedString <* parseChar ',')
  _ <- parseString "\"content\":["
  content <- parseBodyElements ']' <|> parseEmptyList
  _ <- parseString "]}}"
  return (Section title content)

-- | Parses a code block object from JSON format.
-- The function takes a parser and returns a code block element.
parseBodyCodeBlock :: Parser Element
parseBodyCodeBlock = do
  _ <- parseString "{\"codeblock\":["
  content <- parseBodyElements ']' <|> parseEmptyList
  _ <- parseString "]}"
  return (CodeBlock content)

-- | Parses a list object from JSON format.
-- The function takes a parser and returns a list element.
parseBodyList :: Parser Element
parseBodyList = do
  _ <- parseString "{\"list\":["
  items <- parseListItems ']' <|> parseEmptyList
  _ <- parseString "]}"
  return (List items)

-- | Parses the items of a list from JSON format.
-- The function takes an end character and returns a list of elements.
parseListItems :: Char -> Parser [Element]
parseListItems endChar = do
  item <- parseListItem
  next <- (parseChar ',' >> pure ',') <|> pure endChar
  case next of
    ',' -> do
      rest <- parseListItems endChar
      return (item : rest)
    _ -> return [item]
  
-- | Parses a single list item from JSON format.
-- The function takes a parser and returns an element.
parseListItem :: Parser Element
parseListItem = do
  item <- parseBodyElement
  return (Item [item])
