{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- MarkdownBody
-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Markdown.MarkdownBody
    ( parseBody
    , bodyToMarkdown
    ) where

import Control.Applicative (Alternative(..), optional)

import Markdown.MarkdownUtils
import BasicParser
import Document

-- =====================
-- BODY PRINTER
-- =====================

-- | Converts the body of a document to Markdown format.
-- The function takes a list of elements and returns a Markdown string.
bodyToMarkdown :: [Element] -> String
bodyToMarkdown = elementsListToMarkdown 1

-- | Converts a list of elements to Markdown format.
-- The function takes an indentation level and a list of elements.
elementsListToMarkdown :: Int -> [Element] -> String
elementsListToMarkdown depth =
  concatMap (elementToMarkdown depth)

-- | Converts a single element to Markdown format.
-- The function takes an indentation level and an element.
elementToMarkdown :: Int -> Element -> String
elementToMarkdown _ (Text t) =
  t
elementToMarkdown _ (Italic t) =
  "*" ++ t ++ "*"
elementToMarkdown _ (Bold t) =
  "**" ++ t ++ "**"
elementToMarkdown _ (Code t) =
  "`" ++ t ++ "`"
elementToMarkdown depth (Link url content) =
  "[" ++ concatMap (elementToMarkdown depth) content ++ "](" ++ url ++ ")"
elementToMarkdown depth (Image url content) =
  "![" ++ concatMap (elementToMarkdown depth) content ++ "](" ++ url ++ ")"
elementToMarkdown depth (Paragraph elems) =
  concatMap (elementToMarkdown depth) elems ++ "\n"
elementToMarkdown depth (Section (Just "") elems) =
  elementsListToMarkdown (depth + 1) elems
elementToMarkdown depth (Section (Just title) elems) =
  replicate depth '#' ++ " " ++ title ++ "\n" ++
    elementsListToMarkdown (depth + 1) elems
elementToMarkdown _ (CodeBlock elems) =
  "```\n" ++ elementsListToMarkdown 1 elems ++ "\n```\n"
elementToMarkdown depth (List items) =
  concatMap (itemToMarkdown depth) items
elementToMarkdown _ _ = error "Unsupported element type"

-- | Converts a single list item to Markdown format.
-- The function takes an indentation level and an element.
itemToMarkdown :: Int -> Element -> String
itemToMarkdown depth (Item elems) =
  "- "
  ++ concatMap (removeTrailingNewlines . elementToMarkdown depth) elems ++
   "\n"
itemToMarkdown _ e = error $ "Expected Item, got: " ++ show e

-- =====================
-- BODY PARSER
-- =====================

-- | Parses the body of a document from Markdown format.
-- The function takes a parser and returns a list of elements.
parseBody :: Parser [Element]
parseBody = parseBodyElements

-- | Parses a list of elements from Markdown format.
-- The function takes a parser and returns a list of elements.
parseBodyElements :: Parser [Element]
parseBodyElements = do
  _ <- parseWhiteSpace
  maybeElem <- optional parseBodyElement
  case maybeElem of
    Just element -> do
      rest <- parseBodyElements
      return (element : rest)
    Nothing -> return []

-- | Parses a single element from Markdown format.
-- The function tries to parse different types of elements in order.
parseBodyElement :: Parser Element
parseBodyElement =
      parseBodySection
  <|> parseBodyCodeBlock
  <|> parseBodyList
  <|> parseBodyParagraph

-- | Parses a section from Markdown format.
-- The function takes a parser and returns a section element.
parseBodySection :: Parser Element
parseBodySection = do
  _ <- parseSome (parseChar '#')
  _ <- parseChar ' '
  title <- parseUntil '\n'
  Section (Just title) <$> parseBodyElements

-- | Parses a code block from Markdown format.
-- The function takes a parser and returns a code block element.
parseBodyCodeBlock :: Parser Element
parseBodyCodeBlock = do
  _ <- parseString "```"
  _ <- optional (parseChar '\n')
  linesContent <- parseBodyElements
  _ <- optional (parseChar '\n')
  _ <- parseString "```"
  return (CodeBlock linesContent)

-- | Parses a list from Markdown format.
-- The function takes a parser and returns a list element.
parseBodyList :: Parser Element
parseBodyList = do
  items <- parseSome parseBodyListItem
  return (List items)

-- | Parses a single list item from Markdown format.
-- The function takes a parser and returns an element.
parseBodyListItem :: Parser Element
parseBodyListItem = do
  _ <- parseString "- "
  elems <- parseBodyParagraph
  return (Item [elems])

-- | Parses a paragraph from Markdown format.
-- The function takes a parser and returns a paragraph element.
parseBodyParagraph :: Parser Element
parseBodyParagraph = do
  elements <- parseSome parseBodyParagraphContent
  _ <- parseChar '\n'
  return (Paragraph elements)

-- | Parses the content of a paragraph from Markdown format.
-- The function tries to parse different types of content in order.
parseBodyParagraphContent :: Parser Element
parseBodyParagraphContent =
      parseBodyBold
  <|> parseBodyItalic
  <|> parseBodyCode
  <|> parseBodyLink
  <|> parseBodyImage
  <|> parseBodyText

-- | Parses bold text from Markdown format.
-- The function takes a parser and returns a bold element.
parseBodyBold :: Parser Element
parseBodyBold = do
  _ <- parseString "**"
  content <- parseUntilString "**"
  _ <- parseString "**"
  return (Bold content)

-- | Parses italic text from Markdown format.
-- The function takes a parser and returns an italic element.
parseBodyItalic :: Parser Element
parseBodyItalic = do
  _ <- parseChar '*'
  content <- parseUntil '*'
  _ <- parseChar '*'
  return (Italic content)

-- | Parses inline code from Markdown format.
-- The function takes a parser and returns a code element.
parseBodyCode :: Parser Element
parseBodyCode = do
  _ <- parseChar '`'
  content <- parseUntil '`'
  _ <- parseChar '`'
  return (Code content)

-- | Parses a link from Markdown format.
-- The function takes a parser and returns a link element.
parseBodyLink :: Parser Element
parseBodyLink = do
  _ <- parseChar '['
  text <- parseUntil ']'
  _ <- parseString "]("
  url <- parseUntil ')'
  _ <- parseChar ')'
  return (Link url [Text text])

-- | Parses an image from Markdown format.
-- The function takes a parser and returns an image element.
parseBodyImage :: Parser Element
parseBodyImage = do
  _ <- parseString "!["
  alt <- parseUntil ']'
  _ <- parseString "]("
  url <- parseUntil ')'
  _ <- parseChar ')'
  return (Image url [Text alt])

-- | Parses plain text from Markdown format.
-- The function takes a parser and returns a text element.
parseBodyText :: Parser Element
parseBodyText = do
  text <- parseSome parseNotCharSpecial
  return (Text text)
