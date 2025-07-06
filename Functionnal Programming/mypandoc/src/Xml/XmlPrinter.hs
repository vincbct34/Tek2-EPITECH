{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- XmlPrinter
-}

module Xml.XmlPrinter
    ( documentToXml
    ) where

import Xml.XmlHeader
import Xml.XmlUtils
import Xml.XmlBody
import Document

-- ======================
-- XML PRINTER
-- ======================

documentToXml :: Document -> String
documentToXml doc =
  "<document>\n" ++
  indent 1 ++ headerToXml (docHeader doc) ++ "\n" ++
  indent 1 ++ bodyToXml (docBody doc) ++
  "</document>"
