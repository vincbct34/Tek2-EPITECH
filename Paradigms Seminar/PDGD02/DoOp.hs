{-
-- EPITECH PROJECT, 2024
-- DoOp.hs
-- File description:
-- Paradigms Seminar - Second day of Haskell pool
-}

module Main where

import Data.Char (isDigit)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith, exitFailure)

-- Function that checks if a given element is in a list
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs)
  | a == x    = True
  | otherwise = myElem a xs

-- Function that does the division of two integers
safeDiv :: Int -> Int -> Maybe Int
safeDiv a 0 = Nothing
safeDiv a b = Just (a `div` b)

-- Function that returns the nth element of a list
safeNth :: [a] -> Int -> Maybe a
safeNth [] _ = Nothing
safeNth (x:_) 0 = Just x
safeNth (x:xs) n
  | n < 0     = Nothing
  | otherwise = safeNth xs (n - 1)

-- Function that does the addition of two integers
safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc n = maybeDo (+) n (Just 1)

-- Function that looks up a value in a list of tuples
myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup a ((x, y):xs)
  | a == x    = Just y
  | otherwise = myLookup a xs

-- Function that does operations on two Maybe values
maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo f (Just x) (Just y) = Just (f x y)
maybeDo _ _ _ = Nothing

-- Function that reads an integer from a string
readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt ('-':xs)
  | all isDigit xs = Just (read ('-':xs))
  | otherwise = Nothing
readInt string
  | all isDigit string = Just (read string)
  | otherwise = Nothing

-- Function that gets the length of an input line
getLineLength :: IO Int
getLineLength = length <$> getLine

-- Function that prints a string and returns its length
printAndGetLength :: String -> IO Int
printAndGetLength x = putStrLn x >> return (length x) :: IO Int

-- Function that prints a box of size n
printBox :: Int -> IO ()
printBox 1 = putStrLn "++"
printBox n
    | n <= 0    = return ()
    | otherwise = putStrLn (topBottom n)
      >> mapM_ (\_ -> putStrLn middle) [1..(n-2)]
      >> putStrLn (topBottom n)
  where
    topBottom m = "+" ++ replicate (m * 2 - 2) '-' ++ "+"
    middle = "|" ++ replicate (n * 2 - 2) ' ' ++ "|"

-- Function that concatenates n lines
concatLines :: Int -> IO String
concatLines x
  | x <= 0 = return ""
  | otherwise = do
    line <- getLine :: IO String
    next <- concatLines (x - 1)
    return (line ++ next)

-- Function that reads an integer from the input
getInt :: IO (Maybe Int)
getInt = readInt <$> getLine

-- Function that does the addition of two Maybe Int
safeAdd :: Maybe Int -> Maybe Int -> Maybe Int
safeAdd = maybeDo (+)

-- Function that does the subtraction of two Maybe Int
safeSub :: Maybe Int -> Maybe Int -> Maybe Int
safeSub = maybeDo (-)

-- Function that does the multiplication of two Maybe Int
safeMul :: Maybe Int -> Maybe Int -> Maybe Int
safeMul = maybeDo (*)

-- Function that does the modulo of two Maybe Int
safeMod :: Maybe Int -> Maybe Int -> Maybe Int
safeMod _ (Just 0) = Nothing
safeMod a b = maybeDo mod a b

-- Function that does the division of two Maybe Int
safeDivV2 :: Maybe Int -> Maybe Int -> Maybe Int
safeDivV2 _ (Just 0) = Nothing
safeDivV2 a b = maybeDo div a b

-- Function that finds the operation to do
getOperation :: String -> (Maybe Int -> Maybe Int -> Maybe Int)
getOperation string = case string of
    "+" -> safeAdd
    "-" -> safeSub
    "*" -> safeMul
    "/" -> safeDivV2
    "%" -> safeMod
    _ -> \_ _ -> Nothing

-- Function that prints an integer
safePrint :: Maybe Int -> IO ()
safePrint Nothing = exitWith (ExitFailure 84)
safePrint (Just x) = print x

-- Main function
main :: IO ()
main = do
    args <- getArgs
    if length args == 3
      then safePrint (getOperation
        (args !! 1) (readInt (head args)) (readInt (args !! 2)))
    else exitWith (ExitFailure 84)
