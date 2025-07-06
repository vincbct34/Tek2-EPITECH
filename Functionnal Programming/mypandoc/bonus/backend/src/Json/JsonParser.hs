{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- JsonParser (debug)
-}

module Json.JsonParser
  ( jsonToDocument
  ) where

import Json.JsonHeader
import Json.JsonBody
import BasicParser
import Document


-- =====================
-- MAIN PARSER
-- =====================

jsonToDocument :: Parser Document
jsonToDocument = do
  _ <- parseChar '{'
  header <- parseHeader
  _ <- parseChar ','
  body <- parseBody
  _ <- parseChar '}'
  return $ Document header body
