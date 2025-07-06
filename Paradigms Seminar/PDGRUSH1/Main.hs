{-
-- EPITECH PROJECT, 2025
-- Main.hs
-- File description:
-- Paradigms Seminar - Rush of Haskell pool
-}

module Main where

-- Import from Lib.hs and Process.hs
import Lib (readInt)
import Process (applyOperation)

-- Import from the Haskell libraries
import Data.List (sort)
import Control.Monad (foldM)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))

-- Process instructions
processSort :: [String] -> ([Int], [Int]) -> IO ([Int], [Int])
processSort instructions state = foldM (flip applyOperation) state instructions

-- Vérifie si une liste est triée
isSorted :: (Ord a) => [a] -> Bool
isSorted xs = xs == sort xs

main :: IO ()
main = do
    args <- getArgs
    case mapM readInt args of
        Nothing -> exitWith (ExitFailure 84)
        Just l_a -> do
            instructions <- words <$> getLine
            (sortedList, temporaryList) <- processSort instructions (l_a, [])
            if isSorted sortedList && null temporaryList
                then putStrLn "OK"
                else putStrLn $ "KO: " ++ show (sortedList, temporaryList)
