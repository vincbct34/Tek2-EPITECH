{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- Document
-}

module Document
  ( Document(..)
  , Header(..)
  , HeaderContent(..)
  , Element(..)
  ) where

data Document = Document
  { docHeader :: Header
  , docBody   :: [Element]
  } deriving (Show, Eq)

newtype Header = Header
  { headerElements :: [HeaderContent]
  } deriving (Show, Eq)

data HeaderContent
  = Title String
  | Author String
  | Date String
  deriving (Show, Eq)

data Element
  = Text String
  | Italic String
  | Bold String
  | Code String
  | Link String [Element]
  | Image String [Element]
  | Paragraph [Element]
  | Section (Maybe String) [Element]
  | CodeBlock [Element]
  | List [Element]
  | Item [Element]
  deriving (Show, Eq)
