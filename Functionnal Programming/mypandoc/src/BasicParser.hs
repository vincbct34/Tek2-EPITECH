{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- BasicParser
-}

module BasicParser
  ( Parser(..)
  , parseChar
  , parseAnyChar
  , parseOr
  , parseAnd
  , parseAndWith
  , parseMany
  , parseSome
  , parseUInt
  , parseInt
  , parseTuple
  , parseTruple
  , parseString
  , parseEmpty
  , parseEmptyString
  , parseEmptyList
  , parseQuotedString
  , parseUntil
  , parseUntilString
  , parseWhiteSpace
  , parseNotChar
  , parseNotCharSpecial
  ) where

import Control.Applicative (Alternative(..))
import Data.Char (isDigit)

import Document

-- =====================
-- PARSER TYPE
-- =====================

newtype Parser a = Parser {
  runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Nothing -> Nothing
      Just (a, rest) -> Just (f a, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  Parser pf <*> Parser pa = Parser $ \input ->
    case pf input of
      Nothing -> Nothing
      Just (f, rest1) ->
        case pa rest1 of
          Nothing -> Nothing
          Just (a, rest2) -> Just (f a, rest2)

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ \input ->
    case p1 input of
      Nothing -> p2 input
      justRes -> justRes

instance Monad Parser where
  return = pure
  Parser pa >>= f = Parser $ \input ->
    case pa input of
      Nothing -> Nothing
      Just (a, rest1) -> runParser (f a) rest1

-- =====================
-- BASIC PARSERS
-- =====================

parseChar :: Char -> Parser Char
parseChar c = Parser $ \input ->
  case input of
    (x:xs) | x == c -> Just (x, xs)
    _               -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] = empty
parseAnyChar (c:cs) = parseChar c <|> parseAnyChar cs

parseNotChar :: Char -> Parser Char
parseNotChar forbidden = Parser $ \input ->
  case input of
    (x:xs) | x /= forbidden -> Just (x, xs)
    _ -> Nothing

parseWhiteSpace :: Parser String
parseWhiteSpace = parseMany (parseAnyChar " \t\n\r")

parseNotCharSpecial :: Parser Char
parseNotCharSpecial = Parser $ \input ->
  case input of
    (x:xs) | x `notElem` "*`[!" && x /= '\n' -> Just (x, xs)
    _ -> Nothing

-- =====================
-- COMBINATORS
-- =====================

parseOr :: Parser a -> Parser a -> Parser a
parseOr = (<|>)

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = (,) <$> p1 <*> p2

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = f <$> p1 <*> p2

parseMany :: Parser a -> Parser [a]
parseMany = many

parseSome :: Parser a -> Parser [a]
parseSome = some

parseEmpty :: Parser a
parseEmpty = empty

-- =====================
-- NUMERIC PARSERS
-- =====================

parseUInt :: Parser Int
parseUInt = Parser $ \input ->
  let (digits, rest) = span isDigit input
  in if null digits then Nothing else Just (read digits, rest)

parseInt :: Parser Int
parseInt = Parser $ \input ->
  case input of
    ('-':rest) ->
      case runParser parseUInt rest of
        Just (n, rest') -> Just (-n, rest')
        Nothing         -> Nothing
    _ -> runParser parseUInt input

-- =====================
-- STRING PARSERS
-- =====================

parseString :: String -> Parser String
parseString = traverse parseChar

parseEmptyString :: Parser String
parseEmptyString = do
  _ <- parseChar '"'
  _ <- parseChar '"'
  return ""

parseEmptyList :: Parser [Element]
parseEmptyList = do
  _ <- parseChar '['
  _ <- parseChar ']'
  return []

parseQuotedString :: Parser String
parseQuotedString = do
  _ <- parseChar '"'
  content <- parseMany (parseNotChar '"')
  _ <- parseChar '"'
  return content

parseUntil :: Char -> Parser String
parseUntil endChar = Parser $ \input ->
  let (content, rest) = span (/= endChar) input
  in case rest of
      (c:_) | c == endChar -> Just (content, rest)
      [] -> Just (content, [])
      _  -> Nothing
  
parseUntilString :: String -> Parser String
parseUntilString endStr = Parser $ \input ->
  let (content, rest) = span (`notElem` endStr) input
  in case rest of
      (c:_) | c `elem` endStr -> Just (content, rest)
      [] -> Just (content, [])
      _  -> Nothing

-- =====================
-- TUPLE AND TRUPLE PARSERS
-- =====================

parseTuple :: Parser a -> Parser (a, a)
parseTuple p = do
  _ <- parseChar '('
  a <- p
  _ <- parseChar ','
  b <- p
  _ <- parseChar ')'
  return (a, b)

parseTruple :: Parser (Int, Int, Int)
parseTruple = do
  _ <- parseChar '('
  a <- parseInt
  _ <- parseChar ','
  b <- parseInt
  _ <- parseChar ','
  c <- parseInt
  _ <- parseChar ')'
  return (a, b, c)
