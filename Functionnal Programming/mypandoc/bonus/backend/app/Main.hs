{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- Main
-}

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp (setHost, setPort, defaultSettings)
import Network.HTTP.Types.Status (badRequest400)
import Data.List (isPrefixOf)
import Network.Wai (Middleware)
import Web.Scotty

import BasicParser
import Document

import Json.JsonPrinter
import Json.JsonParser

import Xml.XmlPrinter
import Xml.XmlParser

import Markdown.MarkdownPrinter
import Markdown.MarkdownParser
import Markdown.MarkdownUtils

parseJson :: String -> IO (Either String Document)
parseJson jsonFile =
  return $ case runParser jsonToDocument jsonFile of
    Just (doc, "") -> Right doc
    Just (_, _) -> Left "JSON parsing error: leftover data"
    Nothing -> Left "JSON parsing error: parser failed"

parseXml :: String -> IO (Either String Document)
parseXml xmlFile =
  return $ case runParser xmlToDocument xmlFile of
    Just (doc, "") -> Right doc
    Just (_, _) -> Left "XML parsing error: leftover data"
    Nothing -> Left "XML parsing error: parser failed"

parseMarkdown :: String -> IO (Either String Document)
parseMarkdown markdownFile =
  return $ case runParser markdownToDocument
    (normalizeLineEndings markdownFile) of
    Just (doc, "") -> Right doc
    Just (_, _) -> Left "Markdown parsing error: leftover data"
    Nothing -> Left "Markdown parsing error: parser failed"

detectFormat :: String -> Either String String
detectFormat fileContent
  | "{" `isPrefixOf` fileContent = Right "json"
  | "<document>" `isPrefixOf` fileContent = Right "xml"
  | "---" `isPrefixOf` fileContent = Right "markdown"
  | otherwise = Left
    "Unsupported file format: unable to detect format from content"

parseDocument :: String -> IO (Either String Document)
parseDocument fileContent = case detectFormat fileContent of
  Left err -> return $ Left err
  Right format -> case format of
    "json" -> parseJson fileContent
    "xml" -> parseXml fileContent
    "markdown" -> parseMarkdown fileContent
    _ -> return $ Left $ "Unsupported format: " ++ format

handleConvert :: String -> String -> IO (Either String String)
handleConvert fileContent targetFormat = do
  result <- parseDocument fileContent
  return $ case result of
    Left err -> Left $ "Parsing error: " ++ err
    Right doc -> case targetFormat of
      "json" -> Right $ documentToJson doc
      "xml" -> Right $ documentToXml doc
      "markdown" -> Right $ documentToMarkdown doc
      _ -> Left $ "Unsupported format: " ++ targetFormat

main :: IO ()
main = scottyOpts opts $
  middleware corsMiddleware >>
  post "/api/convert" (do
    fileContent <- formParam "file" :: ActionM String
    format <- formParam "format"
    result <- liftIO $ handleConvert fileContent format
    case result of
      Left err -> status badRequest400 >> json err
      Right output ->
        setHeader "Content-Type" "application/json" >> json output)

opts :: Options
opts = Options 1 (setHost "0.0.0.0" $ setPort 3000 defaultSettings)

corsMiddleware :: Middleware
corsMiddleware = cors (const $ Just policy)

policy :: CorsResourcePolicy
policy = simpleCorsResourcePolicy
  { corsOrigins = Nothing
  , corsMethods = ["POST"]
  , corsRequestHeaders = ["Content-Type"]
  }
