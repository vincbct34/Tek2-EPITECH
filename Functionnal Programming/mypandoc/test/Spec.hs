{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- Spec
-}

module Main (main) where

import Test.Hspec

import BasicParser
import CommonUtils (isHeaderValid)
import Document (HeaderContent(..), Element(..), Document(..), Header(..))
import Json.JsonBody
import Json.JsonUtils (jsonUnbeautify, indent)
import Json.JsonPrinter (documentToJson)
import Json.JsonHeader (parseHeader)
import Json.JsonParser (jsonToDocument)
import Markdown.MarkdownBody (parseBody, bodyToMarkdown)

import qualified Json.JsonBody as Json
import qualified Markdown.MarkdownBody as Markdown
import qualified Xml.XmlBody as Xml

main :: IO ()
main = hspec $ do

  -- ==========================
  -- TESTS FOR BASIC PARSER
  -- ==========================

  describe "BasicParser.parseChar" $ do
    it "parses the expected character" $ do
      runParser (parseChar 'a') "abc" `shouldBe` Just ('a', "bc")
    it "fails on a different character" $ do
      runParser (parseChar 'b') "abc" `shouldBe` Nothing
    it "fails on an empty input" $ do
      runParser (parseChar 'a') "" `shouldBe` Nothing

  describe "BasicParser.parseAnyChar" $ do
    it "parses any character from the list" $ do
      runParser (parseAnyChar "abc") "abc" `shouldBe` Just ('a', "bc")
    it "fails if no character matches" $ do
      runParser (parseAnyChar "xyz") "abc" `shouldBe` Nothing
    it "fails on an empty input" $ do
      runParser (parseAnyChar "abc") "" `shouldBe` Nothing

  describe "BasicParser.parseNotChar" $ do
    it "parses a character not matching the forbidden one" $ do
      runParser (parseNotChar 'a') "bcd" `shouldBe` Just ('b', "cd")
    it "fails if the first character matches the forbidden one" $ do
      runParser (parseNotChar 'a') "abc" `shouldBe` Nothing
    it "fails on an empty input" $ do
      runParser (parseNotChar 'a') "" `shouldBe` Nothing

  describe "BasicParser.parseWhiteSpace" $ do
    it "parses consecutive whitespace characters" $ do
      runParser parseWhiteSpace "   abc" `shouldBe` Just ("   ", "abc")
    it "fails if no whitespace is present" $ do
      runParser parseWhiteSpace "abc" `shouldBe` Just ("", "abc")
    it "parses empty input as empty whitespace" $ do
      runParser parseWhiteSpace "" `shouldBe` Just ("", "")

  describe "BasicParser.parseNotCharSpecial" $ do
    it "parses a character not in the special set" $ do
      runParser parseNotCharSpecial "abc" `shouldBe` Just ('a', "bc")
    it "fails if the first character is in the special set" $ do
      runParser parseNotCharSpecial "*abc" `shouldBe` Nothing
    it "fails on an empty input" $ do
      runParser parseNotCharSpecial "" `shouldBe` Nothing

  -- ==========================
  -- TESTS FOR COMBINATORS
  -- ==========================

  describe "BasicParser.parseOr" $ do
    it "parses using the first parser if it succeeds" $ do
      runParser (parseOr (parseChar 'a') (parseChar 'b')) "abc" `shouldBe` Just ('a', "bc")
    it "parses using the second parser if the first fails" $ do
      runParser (parseOr (parseChar 'b') (parseChar 'a')) "abc" `shouldBe` Just ('a', "bc")
    it "fails if both parsers fail" $ do
      runParser (parseOr (parseChar 'x') (parseChar 'y')) "abc" `shouldBe` Nothing

  describe "BasicParser.parseAnd" $ do
    it "parses two consecutive elements" $ do
      runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abc" `shouldBe` Just (('a', 'b'), "c")
    it "fails if the first parser fails" $ do
      runParser (parseAnd (parseChar 'x') (parseChar 'b')) "abc" `shouldBe` Nothing
    it "fails if the second parser fails" $ do
      runParser (parseAnd (parseChar 'a') (parseChar 'x')) "abc" `shouldBe` Nothing

  describe "BasicParser.parseAndWith" $ do
    it "combines results using a custom function" $ do
      runParser (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "abc" `shouldBe` Just ("ab", "c")
    it "fails if the first parser fails" $ do
      runParser (parseAndWith (\x y -> [x, y]) (parseChar 'x') (parseChar 'b')) "abc" `shouldBe` Nothing
    it "fails if the second parser fails" $ do
      runParser (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'x')) "abc" `shouldBe` Nothing

  describe "BasicParser.parseMany" $ do
    it "parses zero or more occurrences of a parser" $ do
      runParser (parseMany (parseChar 'a')) "aaabc" `shouldBe` Just ("aaa", "bc")
    it "parses an empty result if no matches are found" $ do
      runParser (parseMany (parseChar 'x')) "abc" `shouldBe` Just ("", "abc")

  describe "BasicParser.parseSome" $ do
    it "parses one or more occurrences of a parser" $ do
      runParser (parseSome (parseChar 'a')) "aaabc" `shouldBe` Just ("aaa", "bc")
    it "fails if no matches are found" $ do
      runParser (parseSome (parseChar 'x')) "abc" `shouldBe` Nothing

  -- ==========================
  -- TESTS FOR NUMERIC PARSERS
  -- ==========================

  describe "BasicParser.parseUInt" $ do
    it "parses a positive integer" $ do
      runParser parseUInt "123abc" `shouldBe` Just (123, "abc")
    it "fails if no digits are present" $ do
      runParser parseUInt "abc" `shouldBe` Nothing
    it "parses an empty input as failure" $ do
      runParser parseUInt "" `shouldBe` Nothing

  describe "BasicParser.parseInt" $ do
    it "parses a positive integer" $ do
      runParser parseInt "123abc" `shouldBe` Just (123, "abc")
    it "parses a negative integer" $ do
      runParser parseInt "-123abc" `shouldBe` Just (-123, "abc")
    it "fails if no digits are present" $ do
      runParser parseInt "abc" `shouldBe` Nothing
    it "fails on a standalone minus sign" $ do
      runParser parseInt "-abc" `shouldBe` Nothing
    it "parses an empty input as failure" $ do
      runParser parseInt "" `shouldBe` Nothing

  -- ==========================
  -- TESTS FOR STRING PARSERS
  -- ==========================

  describe "BasicParser.parseString" $ do
    it "parses an exact string" $ do
      runParser (parseString "hello") "hello world" `shouldBe` Just ("hello", " world")
    it "fails if the string does not match" $ do
      runParser (parseString "hello") "world" `shouldBe` Nothing
    it "fails on an empty input" $ do
      runParser (parseString "hello") "" `shouldBe` Nothing

  describe "BasicParser.parseEmptyString" $ do
    it "parses an empty string enclosed in quotes" $ do
      runParser parseEmptyString "\"\"" `shouldBe` Just ("", "")
    it "fails if the input is not an empty quoted string" $ do
      runParser parseEmptyString "\"abc\"" `shouldBe` Nothing

  describe "BasicParser.parseEmptyList" $ do
    it "parses an empty list" $ do
      runParser parseEmptyList "[]" `shouldBe` Just ([], "")
    it "fails if the input is not an empty list" $ do
      runParser parseEmptyList "[1,2,3]" `shouldBe` Nothing

  describe "BasicParser.parseQuotedString" $ do
    it "parses a string enclosed in quotes" $ do
      runParser parseQuotedString "\"hello\"" `shouldBe` Just ("hello", "")
    it "parses an empty quoted string" $ do
      runParser parseQuotedString "\"\"" `shouldBe` Just ("", "")
    it "fails if the input is not a quoted string" $ do
      runParser parseQuotedString "hello" `shouldBe` Nothing

  describe "BasicParser.parseUntil" $ do
    it "parses until a specific character" $ do
      runParser (parseUntil ',') "hello,world" `shouldBe` Just ("hello", ",world")
    it "parses the entire input if the character is not found" $ do
      runParser (parseUntil ',') "hello" `shouldBe` Just ("hello", "")
    it "parses an empty input as an empty result" $ do
      runParser (parseUntil ',') "" `shouldBe` Just ("", "")

  describe "BasicParser.parseUntilString" $ do
    it "parses until one of the specified characters" $ do
      runParser (parseUntilString ",.") "hello,world" `shouldBe` Just ("hello", ",world")
    it "parses the entire input if none of the characters are found" $ do
      runParser (parseUntilString ",.") "hello" `shouldBe` Just ("hello", "")
    it "parses an empty input as an empty result" $ do
      runParser (parseUntilString ",.") "" `shouldBe` Just ("", "")

  -- ==========================
  -- TESTS FOR TUPLE AND TRUPLE PARSERS
  -- ==========================

  describe "BasicParser.parseTuple" $ do
    it "parses a tuple of two elements" $ do
      runParser (parseTuple parseInt) "(1,2)" `shouldBe` Just ((1, 2), "")
    it "fails if the input is not a valid tuple" $ do
      runParser (parseTuple parseInt) "(1,)" `shouldBe` Nothing
    it "fails if the input is missing parentheses" $ do
      runParser (parseTuple parseInt) "1,2" `shouldBe` Nothing
    it "fails if the input is empty" $ do
      runParser (parseTuple parseInt) "" `shouldBe` Nothing

  describe "BasicParser.parseTruple" $ do
    it "parses a tuple of three integers" $ do
      runParser parseTruple "(1,2,3)" `shouldBe` Just ((1, 2, 3), "")
    it "fails if the input is not a valid truple" $ do
      runParser parseTruple "(1,2,)" `shouldBe` Nothing
    it "fails if the input is missing parentheses" $ do
      runParser parseTruple "1,2,3" `shouldBe` Nothing
    it "fails if the input is empty" $ do
      runParser parseTruple "" `shouldBe` Nothing

  -- ==========================
  -- TESTS FOR COMMON UTILS
  -- ==========================

  describe "CommonUtils.isHeaderValid" $ do
    it "returns True for a valid header with one title, one author, and one date" $ do
      isHeaderValid [Title "Title", Author "Author", Date "Date"] `shouldBe` True

    it "returns True for a valid header with one title and one author" $ do
      isHeaderValid [Title "Title", Author "Author"] `shouldBe` True

    it "returns True for a valid header with only one title" $ do
      isHeaderValid [Title "Title"] `shouldBe` True

    it "returns False for a header with more than one title" $ do
      isHeaderValid [Title "Title1", Title "Title2"] `shouldBe` False

    it "returns False for a header with more than one author" $ do
      isHeaderValid [Title "Title", Author "Author1", Author "Author2"] `shouldBe` False

    it "returns False for a header with more than one date" $ do
      isHeaderValid [Title "Title", Date "Date1", Date "Date2"] `shouldBe` False

    it "returns False for a header with more than three elements" $ do
      isHeaderValid [Title "Title", Author "Author", Date "Date", Title "Extra"] `shouldBe` False

    it "returns False for an empty header" $ do
      isHeaderValid [] `shouldBe` False

  -- ==========================
  -- TESTS FOR JSON
  -- ==========================

  describe "Json.JsonBody.parseBody" $ do
    it "parses a body with a single text element" $ do
      runParser Json.parseBody "\"body\":[\"Sample\"]" `shouldBe` Just ([Text "Sample"], "")

    it "parses a body with multiple elements" $ do
      runParser Json.parseBody "\"body\":[\"Hello\",{\"bold\":\"World\"}]" `shouldBe` Just ([Text "Hello", Bold "World"], "")

  -- ==========================
  -- TESTS FOR JSON BODY PRINTER
  -- ==========================

  describe "Json.JsonBody.bodyToJson" $ do
    it "converts a body with a single text element to JSON" $ do
      let body = [Text "Sample text"]
      bodyToJson body `shouldBe` "\"body\": [\n        \"Sample text\"\n    ]"

    it "converts a body with multiple elements to JSON" $ do
      let body = [Text "Hello", Bold "World"]
      bodyToJson body `shouldBe` "\"body\": [\n        \"Hello\",\n        {\n            \"bold\": \"World\"\n        }\n    ]"

    it "converts an empty body to JSON" $ do
      let body = []
      bodyToJson body `shouldBe` "\"body\": [\n\n    ]"

    it "converts a body with nested elements to JSON" $ do
      let body = [Paragraph [Text "Nested", Bold "Content"]]
      bodyToJson body `shouldBe` "\"body\": [\n        [\n            \"Nested\",\n            {\n                \"bold\": \"Content\"\n            }\n        ]\n    ]"

  -- ==========================
  -- TESTS FOR JSON UTILS
  -- ==========================

  describe "Json.JsonUtils.jsonUnbeautify" $ do
    it "removes spaces, tabs, and newlines outside of strings" $ do
      jsonUnbeautify "{\n  \"key\": \"value\"  }" `shouldBe` "{\"key\":\"value\"}"
    it "preserves spaces inside strings" $ do
      jsonUnbeautify "{ \"key\": \"a value with spaces\" }" `shouldBe` "{\"key\":\"a value with spaces\"}"
    it "handles empty input" $ do
      jsonUnbeautify "" `shouldBe` ""

  describe "Json.JsonUtils.indent" $ do
    it "returns the correct indentation for level 0" $ do
      indent 0 `shouldBe` ""
    it "returns the correct indentation for level 1" $ do
      indent 1 `shouldBe` "    "
    it "returns the correct indentation for level 3" $ do
      indent 3 `shouldBe` "            "

  -- ==========================
  -- TESTS FOR JSON PRINTER
  -- ==========================

  describe "Json.JsonPrinter.documentToJson" $ do
    it "converts a document with a header and body to JSON" $ do
      let doc = Document
                  { docHeader = Header [Title "Sample Title", Author "Author Name"]
                  , docBody = [Text "Sample body text"]
                  }
      documentToJson doc `shouldBe` "{\n    \"header\": {\n        \"title\": \"Sample Title\",\n        \"author\": \"Author Name\"\n    },\n    \"body\": [\n        \"Sample body text\"\n    ]\n}"

    it "handles an empty document" $ do
      let doc = Document { docHeader = Header [], docBody = [] }
      documentToJson doc `shouldBe` "{\n    \"header\": {\n\n    },\n    \"body\": [\n\n    ]\n}"

  -- ==========================
  -- TESTS FOR JSON HEADER PARSER
  -- ==========================

  describe "Json.JsonHeader.parseHeader" $ do
    it "parses a valid header with title, author, and date" $ do
      runParser parseHeader "\"header\":{\"title\":\"Sample Title\",\"author\":\"Author Name\",\"date\":\"2025-01-01\"}" 
        `shouldBe` Just (Header [Title "Sample Title", Author "Author Name", Date "2025-01-01"], "")

    it "parses a valid header with only a title" $ do
      runParser parseHeader "\"header\":{\"title\":\"Sample Title\"}" 
        `shouldBe` Just (Header [Title "Sample Title"], "")

    it "fails to parse an invalid header with duplicate titles" $ do
      runParser parseHeader "\"header\":{\"title\":\"Title1\",\"title\":\"Title2\"}" 
        `shouldBe` Nothing

  -- ==========================
  -- TESTS FOR JSON PARSER
  -- ==========================

  describe "Json.JsonParser.jsonToDocument" $ do
    it "fails to parse an invalid JSON document" $ do
      let json = "{\n\"header\":{\"title\":\"Sample Title\"},\n\"body\":\"Invalid body\"\n}"
      runParser jsonToDocument json `shouldBe` Nothing

  -- ==========================
  -- TESTS FOR MARKDOWN BODY PARSER
  -- ==========================

  describe "Markdown.MarkdownBody.parseBody" $ do
    it "parses a body with a single text element" $ do
      runParser Markdown.parseBody "\nSample text\n" `shouldBe` Just ([Paragraph [Text "Sample text"]], "")

    it "parses a body with multiple elements" $ do
      let input = "\n# Title\n\nSample text\n\n- Item 1\n- Item 2\n"
      runParser Markdown.parseBody input `shouldBe` Just ([Section (Just "Title") [Paragraph [Text "Sample text"], List [Paragraph [Text "Item 1"], Paragraph [Text "Item 2"]]]], "")

    it "parses an empty body" $ do
      runParser Markdown.parseBody "\n" `shouldBe` Just ([], "")

    it "parses a body with nested elements" $ do
      let input = "\n# Title\n\n## Subtitle\n\nSample text\n"
      runParser Markdown.parseBody input `shouldBe` Just ([Section (Just "Title") [Section (Just "Subtitle") [Paragraph [Text "Sample text"]]]], "")

  -- ==========================
  -- TESTS FOR MARKDOWN BODY PRINTER
  -- ==========================

  describe "Markdown.MarkdownBody.bodyToMarkdown" $ do
    it "converts a body with a single text element to Markdown" $ do
      let body = [Paragraph [Text "Sample text"]]
      bodyToMarkdown body `shouldBe` "Sample text\n"

    it "converts a body with multiple elements to Markdown" $ do
      let body = [Section (Just "Title") [Paragraph [Text "Sample text"], List [Paragraph [Text "Item 1"], Paragraph [Text "Item 2"]]]]
      bodyToMarkdown body `shouldBe` "# Title\nSample text\n- Item 1\n- Item 2\n"

    it "converts an empty body to Markdown" $ do
      let body = []
      bodyToMarkdown body `shouldBe` ""

    it "converts a body with nested elements to Markdown" $ do
      let body = [Section (Just "Title") [Section (Just "Subtitle") [Paragraph [Text "Sample text"]]]]
      bodyToMarkdown body `shouldBe` "# Title\n## Subtitle\nSample text\n"
