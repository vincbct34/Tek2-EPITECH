{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- Main
-}

module Main (main) where

import Control.Exception (catch, IOException)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import Data.List (isSuffixOf)

import BasicParser
import Document

import Json.JsonPrinter
import Json.JsonParser
import Json.JsonUtils

import Markdown.MarkdownPrinter
import Markdown.MarkdownParser
import Markdown.MarkdownUtils

import Xml.XmlPrinter
import Xml.XmlParser

main :: IO ()
main = do
  args <- getArgs
  case parseOptions args of
    Just opts -> runWithOptions opts
    Nothing ->
      hPutStrLn stderr usage >> exitWith (ExitFailure 84)

-- =====================
-- PARSING OPTIONS
-- =====================

data Options = Options
  { optInput     :: FilePath
  , optOutput    :: Maybe FilePath
  , optOutFormat :: String
  , optInFormat  :: Maybe String
  }

parseOptions :: [String] -> Maybe Options
parseOptions args = do
  input <- getOptValue "-i" args
  output <- Just (getOptValue "-o" args)
  outFormat <- getOptValue "-f" args
  let inFormat = getOptValue "-e" args
  return $ Options input output outFormat inFormat

getOptValue :: String -> [String] -> Maybe String
getOptValue _ [] = Nothing
getOptValue key (k:v:rest)
  | key == k  = Just v
  | otherwise = getOptValue key (v:rest)
getOptValue _ _ = Nothing

-- =====================
-- CORE LOGIC
-- =====================

runWithOptions :: Options -> IO ()
runWithOptions opts =
  catch (do
    content <- readFile (optInput opts)
    case detectFormat (optInFormat opts) (optInput opts) of
      Just "json"     -> handleJson content opts
      Just "xml"      -> handleXml content opts
      Just "markdown" -> handleMarkdown content opts
      _ -> hPutStrLn stderr "Unsupported or undetectable input format" >>
           exitWith (ExitFailure 84))
  handleReadError

handleJson :: String -> Options -> IO ()
handleJson content opts =
  case runParser jsonToDocument (jsonUnbeautify content) of
    Just (doc, "") -> outputDocument (optOutFormat opts) (optOutput opts) doc
    Just (parsed, rest) -> failWith $ "Failed to parse JSON, input parsed: "
                                 ++ show parsed ++ ", remaining input: "
                                 ++ rest
    Nothing -> failWith "Failed to parse JSON"

handleXml :: String -> Options -> IO ()
handleXml content opts = 
  case runParser xmlToDocument content of
    Just (doc, "") -> outputDocument (optOutFormat opts) (optOutput opts) doc
    Just (parsed, rest) -> failWith $ "Failed to parse XML, input parsed: "
                                 ++ show parsed ++ ", remaining input: "
                                 ++ rest
    Nothing -> failWith "Failed to parse XML"

handleMarkdown :: String -> Options -> IO ()
handleMarkdown content opts =
  case runParser markdownToDocument (markdownUnbeautify content) of
    Just (doc, "") -> outputDocument (optOutFormat opts) (optOutput opts) doc
    Just (parsed, rest) -> failWith $ "Failed to parse Markd, input parsed: "
                                     ++ show parsed ++ ", remaining input: "
                                     ++ rest
    Nothing -> failWith "Failed to parse Markdown"

outputDocument :: String -> Maybe FilePath -> Document -> IO ()
outputDocument "json" Nothing doc =
  putStrLn (documentToJson doc)
outputDocument "json" (Just path) doc =
  writeFile path (documentToJson doc)
    `catch` handleWriteError
outputDocument "xml" Nothing doc =
  putStrLn (documentToXml doc)
outputDocument "xml" (Just path) doc =
  writeFile path (documentToXml doc)
    `catch` handleWriteError
outputDocument "markdown" Nothing doc =
  putStrLn (documentToMarkdown doc)
outputDocument "markdown" (Just path) doc =
  writeFile path (documentToMarkdown doc)
    `catch` handleWriteError
outputDocument _ _ _ = failWith "Unknown output format"

failWith :: String -> IO ()
failWith msg =
  hPutStrLn stderr msg >>
  exitWith (ExitFailure 84)

-- =====================
-- FORMAT DETECTION
-- =====================

detectFormat :: Maybe String -> FilePath -> Maybe String
detectFormat (Just f) _ = Just f
detectFormat Nothing path
  | ".json" `isSuffixOf` path = Just "json"
  | ".xml" `isSuffixOf` path  = Just "xml"
  | ".md" `isSuffixOf` path   = Just "markdown"
  | otherwise = Nothing

-- =====================
-- USAGE MESSAGE
-- =====================

usage :: String
usage = unlines
  [ "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]"
  , ""
  , "  ifile     path to the file to convert"
  , "  oformat   output format (xml, json, markdown)"
  , "  ofile     path to the output file"
  , "  iformat   input format (xml, json, markdown)"
  ]

-- =====================
-- UTILITY FUNCTIONS
-- =====================

handleWriteError :: IOException -> IO ()
handleWriteError e =
  hPutStrLn stderr ("Failed to write file: " ++ show e) >>
  exitWith (ExitFailure 84)

handleReadError :: IOException -> IO ()
handleReadError e =
  hPutStrLn stderr ("Failed to read file: " ++ show e) >>
  exitWith (ExitFailure 84)