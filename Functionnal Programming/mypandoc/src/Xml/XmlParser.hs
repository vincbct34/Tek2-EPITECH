{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- XmlParser
-}

module Xml.XmlParser
    ( xmlToDocument
    ) where

import Control.Applicative (optional)

import Xml.XmlHeader
import Xml.XmlBody
import BasicParser
import Document

-- =====================
-- MAIN PARSER
-- =====================

xmlToDocument :: Parser Document
xmlToDocument = do
  _ <- parseString "<document>"
  _ <- optional parseWhiteSpace
  header <- parseHeader
  _ <- optional parseWhiteSpace
  body <- parseBody
  _ <- optional parseWhiteSpace
  _ <- parseString "</document>"
  return (Document header body)
