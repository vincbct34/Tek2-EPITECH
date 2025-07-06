{-
-- EPITECH PROJECT, 2025
-- Lib.hs
-- File description:
-- Paradigms Seminar - Rush of Haskell pool
-}

module Lib where

import Data.Char (isDigit)

-- Function that reads an integer from a string
readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt ('-':xs)
  | all isDigit xs = Just (read ('-':xs))
  | otherwise = Nothing
readInt string
  | all isDigit string = Just (read string)
  | otherwise = Nothing
