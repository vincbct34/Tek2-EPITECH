{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- XmlBody
-}

module Xml.XmlBody
    ( parseBody
    , bodyToXml
    ) where

import Control.Applicative (Alternative(..), optional)
import Data.List (intercalate)

import Xml.XmlUtils
import BasicParser
import Document

-- =====================
-- BODY PRINTER
-- =====================

-- | Converts the body of a document to XML format.
-- The function takes a list of elements and returns an XML string.
bodyToXml :: [Element] -> String
bodyToXml elems =
  "<body>\n" ++
  elementsListToXml 2 elems ++ indent 1 ++
  "</body>\n"

-- | Converts a list of elements to XML format.
-- The function takes an indentation level and a list of elements.
elementsListToXml :: Int -> [Element] -> String
elementsListToXml tab elems =
  let xmlElems = map (elementToXml tab) elems
  in unlines xmlElems

-- | Converts a single element to XML format.
-- The function takes an indentation level and an element.
elementToXml :: Int -> Element -> String
elementToXml tab (Paragraph content) =
  indent tab ++ "<paragraph>" ++
  concatMap (inlineToXml 0) content ++
  "</paragraph>"
elementToXml tab (Section (Just "") content) =
  indent tab ++ "<section title=\"\">\n" ++
  elementsListToXml (tab + 1) content ++
  indent tab ++ "</section>"
elementToXml tab (Section (Just title) content) =
  indent tab ++ "<section title=\"" ++ title ++ "\">\n" ++
  elementsListToXml (tab + 1) content ++
  indent tab ++ "</section>"
elementToXml tab (CodeBlock elems) =
  indent tab ++ "<codeblock>\n" ++
  elementsListToXml (tab + 1) elems ++
  indent tab ++ "</codeblock>"
elementToXml tab (List items) =
  indent tab ++ "<list>\n" ++
  intercalate "\n" (map (itemToXml (tab + 1)) items) ++ "\n" ++
  indent tab ++ "</list>"
elementToXml _ (Bold content)   = "<bold>" ++ content ++ "</bold>"
elementToXml _ (Italic content) = "<italic>" ++ content ++ "</italic>"
elementToXml _ (Code content)   = "<code>" ++ content ++ "</code>"
elementToXml _ (Link url content) =
  "<link url=\"" ++ url ++ "\">"
  ++ concatMap (inlineToXml 0) content ++
  "</link>"
elementToXml _ (Image url content) =
  "<image url=\"" ++ url
  ++ "\">" ++ concatMap (inlineToXml 0) content ++
  "</image>"
elementToXml _ (Text content) = content
elementToXml _ _ = error "Unsupported element"

-- | Converts a single list item to XML format.
-- The function takes an indentation level and an element.
itemToXml :: Int -> Element -> String
itemToXml tab (Item elems) =
  intercalate "\n" (map (elementToXml tab) elems)
itemToXml _ e =
  error $ "Expected Item in list, got: " ++ show e

-- =====================
-- HELPERS
-- =====================

-- | Converts inline elements to XML format.
-- The function takes an indentation level and an element.
inlineToXml :: Int -> Element -> String
inlineToXml _ = elementToXml 0

-- =====================
-- BODY PARSER
-- =====================

-- | Parses the body of a document from XML format.
-- The function takes a parser and returns a list of elements.
parseBody :: Parser [Element]
parseBody = do
  _ <- parseString "<body>"
  _ <- optional parseWhiteSpace
  elems <- parseBodyElements
  _ <- parseString "</body>"
  _ <- optional parseWhiteSpace
  return elems

-- | Parses a list of elements from XML format.
-- The function takes a parser and returns a list of elements.
parseBodyElements :: Parser [Element]
parseBodyElements =
  (parseBodyElement >>= \element ->
    optional parseWhiteSpace >>
    (element :) <$> parseBodyElements)
  <|> pure []

-- | Parses a single element from XML format.
-- The function tries to parse different types of elements in order.
parseBodyElement :: Parser Element
parseBodyElement =
    parseBodyParagraph <|> parseBodySection
  <|> parseBodyCodeBlock <|> parseBodyList

-- | Parses a paragraph from XML format.
-- The function takes a parser and returns a paragraph element.
parseBodyParagraph :: Parser Element
parseBodyParagraph = do
  _ <- parseString "<paragraph>"
  content <- parseSome parseSimpleContent
  _ <- parseString "</paragraph>"
  return (Paragraph content)

-- | Parses simple content from XML format.
-- The function tries to parse different types of inline content in order.
parseSimpleContent :: Parser Element
parseSimpleContent =
      parseBodyBold
  <|> parseBodyItalic
  <|> parseBodyCode
  <|> parseBodyLink
  <|> parseBodyImage
  <|> parseBodyText

-- | Parses a section from XML format.
-- The function takes a parser and returns a section element.
parseBodySection :: Parser Element
parseBodySection = do
  _ <- parseString "<section"
  title <- optional (do
    _ <- parseString " title="
    parseQuotedString)
  _ <- parseChar '>'
  _ <- optional parseWhiteSpace
  content <- parseBodyElements
  _ <- parseString "</section>"
  return (Section title content)

-- | Parses a code block from XML format.
-- The function takes a parser and returns a code block element.
parseBodyCodeBlock :: Parser Element
parseBodyCodeBlock = do
  _ <- parseString "<codeblock>"
  _ <- optional parseWhiteSpace
  content <- parseBodyElements
  _ <- parseString "</codeblock>"
  return (CodeBlock content)

-- | Parses a list from XML format.
-- The function takes a parser and returns a list element.
parseBodyList :: Parser Element
parseBodyList = do
  _ <- parseString "<list>"
  _ <- optional parseWhiteSpace
  items <- parseSome parseListItem
  _ <- parseString "</list>"
  return (List items)

-- | Parses a single list item from XML format.
-- The function takes a parser and returns an element.
parseListItem :: Parser Element
parseListItem = do
  item <- parseBodyParagraph
  _ <- optional parseWhiteSpace
  return (Item [item])

-- | Parses bold text from XML format.
-- The function takes a parser and returns a bold element.
parseBodyBold :: Parser Element
parseBodyBold = do
  _ <- parseString "<bold>"
  content <- parseUntil '<'
  _ <- parseString "</bold>"
  return (Bold content)

-- | Parses italic text from XML format.
-- The function takes a parser and returns an italic element.
parseBodyItalic :: Parser Element
parseBodyItalic = do
  _ <- parseString "<italic>"
  content <- parseUntil '<'
  _ <- parseString "</italic>"
  return (Italic content)

-- | Parses inline code from XML format.
-- The function takes a parser and returns a code element.
parseBodyCode :: Parser Element
parseBodyCode = do
  _ <- parseString "<code>"
  content <- parseUntil '<'
  _ <- parseString "</code>"
  return (Code content)

-- | Parses a link from XML format.
-- The function takes a parser and returns a link element.
parseBodyLink :: Parser Element
parseBodyLink = do
  _ <- parseString "<link url="
  url <- parseQuotedString
  _ <- parseString ">"
  content <- parseUntil '<'
  _ <- parseString "</link>"
  return (Link url [Text content])

-- | Parses an image from XML format.
-- The function takes a parser and returns an image element.
parseBodyImage :: Parser Element
parseBodyImage = do
  _ <- parseString "<image url="
  url <- parseQuotedString
  _ <- parseString ">"
  content <- parseUntil '<'
  _ <- parseString "</image>"
  return (Image url [Text content])

-- | Parses plain text from XML format.
-- The function takes a parser and returns a text element.
parseBodyText :: Parser Element
parseBodyText = do
  content <- parseSome (parseNotChar '<')
  return (Text content)
