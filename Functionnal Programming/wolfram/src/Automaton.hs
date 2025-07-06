{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- Automaton.hs
-}

module Automaton (
    AutomatonConfig(..),
    applyRule,
    createNextGeneration,
    displayGen,
    generationProcess,
    skipGenerations,
    createFirstLine,
    generateInitialState,
    runAutomaton
) where

-- Imports from the standard library
import Data.Bits (shiftL, shiftR, (.&.), (.|.))

-- AutomatonConfig data type to store the configuration of the automaton
data AutomatonConfig = AutomatonConfig {
    rule     :: Int,
    start    :: Int,
    numLines :: Maybe Int,
    window   :: Int,
    move     :: Int
}

-- Function to apply a given rule
applyRule :: Int -> (Int, Int, Int) -> Int
applyRule ruleValue (l, c, r) =
    let index = (l `shiftL` 2) .|. (c `shiftL` 1) .|. r
    in (ruleValue `shiftR` index) .&. 1

-- Function to compute the next generation with dynamic padding
createNextGeneration :: Int -> [Int] -> [Int]
createNextGeneration ruleValue cells =
    let paddedCells = [0, 0] ++ cells ++ [0, 0]
        triplets = zip3 paddedCells (drop 1 paddedCells) (drop 2 paddedCells)
    in map (applyRule ruleValue) triplets

-- Function to format the generation by limiting it to the window size
displayGen :: Int -> [Int] -> String
displayGen win gen =
    let startIdx    = max 0 ((length gen - win) `div` 2)
        cell        = take win (drop startIdx gen)
    in map (\c -> if c == 1 then '*' else ' ') cell

-- Function to generate n generations
generationProcess :: Int -> [Int] -> Maybe Int -> Int -> [String]
generationProcess _ _ (Just 0) _ = []
generationProcess ruleValue gen Nothing win =
    let newLine = createNextGeneration ruleValue gen
    in displayGen win gen : generationProcess ruleValue newLine Nothing win
generationProcess ruleValue gen (Just n) win =
    let newLine = createNextGeneration ruleValue gen
    in displayGen win gen : generationProcess   ruleValue
                                                newLine (Just (n - 1)) win

-- Function to skip n generations
skipGenerations :: Int -> Int -> [Int] -> [Int]
skipGenerations 0 _ line = line
skipGenerations n ruleValue line =
    skipGenerations (n - 1) ruleValue (createNextGeneration ruleValue line)

-- Function to create the initial line
createFirstLine :: AutomatonConfig -> [Int]
createFirstLine config =
    let middle = window config `div` 2
    in replicate (middle + move config) 0
        ++ [1]
        ++ replicate (if even (window config)
                      then middle - move config - 1
                      else middle - move config) 0

-- Function to generate the initial state
generateInitialState :: AutomatonConfig -> [Int]
generateInitialState config =
    let firstLine = createFirstLine config
    in skipGenerations (start config) (rule config) firstLine

-- Function to run the automaton
runAutomaton :: AutomatonConfig -> IO ()
runAutomaton config =
    let binaryRule  = rule config
        startLine   = generateInitialState config
        generations = generationProcess binaryRule startLine 
                                        (numLines config) (window config)
    in mapM_ putStrLn generations
