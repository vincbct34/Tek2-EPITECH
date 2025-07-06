{-
-- EPITECH PROJECT, 2025
-- MyPandoc [WSL: Ubuntu-24.04]
-- File description:
-- CommonUtils
-}

module CommonUtils
    ( isHeaderValid
    ) where

import Document

-- =====================
-- HEADER VALIDATION
-- =====================

isHeaderValid :: [HeaderContent] -> Bool
isHeaderValid elems =
  let titleCount = length [() | Title _ <- elems]
      authorCount = length [() | Author _ <- elems]
      dateCount = length [() | Date _ <- elems]
  in length elems <= 3 && titleCount == 1 && authorCount <= 1 && dateCount <= 1
