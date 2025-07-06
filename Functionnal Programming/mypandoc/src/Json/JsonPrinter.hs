{- 
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- JsonPrinter (corrected)
-}

module Json.JsonPrinter
    ( documentToJson
    ) where

import Json.JsonHeader
import Json.JsonUtils
import Json.JsonBody
import Document

-- =====================
-- JSON PRINTER
-- =====================

documentToJson :: Document -> String
documentToJson doc =
    "{\n" ++
    indent 1 ++ headerToJson (docHeader doc) ++ ",\n" ++
    indent 1 ++ bodyToJson (docBody doc) ++
    "\n}"
