{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- Parser.hs
-}

module Parser (
    parseArgs,
    validateArgs,
    lookupFlag,
    readMaybe
) where

-- Imports from standard libraries
import Data.Maybe   (fromMaybe)

-- Imports from internal modules
import Automaton    (AutomatonConfig(..))

-- Parse the arguments
parseArgs :: [String] -> Either String AutomatonConfig
parseArgs args = do
    checkUnknownArgs args
    ruleVal <-  maybe (Left "Invalid or missing --rule value")
                Right (lookupFlag "--rule" args >>= readMaybe)
    let startVal    = fromMaybe 0 (lookupFlag "--start" args >>= readMaybe)
        numLinesVal = lookupFlag "--lines" args >>= readMaybe
        widthVal    = fromMaybe 80 (lookupFlag "--window" args >>= readMaybe)
        moveVal     = fromMaybe 0 (lookupFlag "--move" args >>= readMaybe)
    validateArgs ruleVal startVal numLinesVal widthVal moveVal

-- Validate the arguments
validateArgs :: Int -> Int -> Maybe Int -> Int -> Int -> Either String AutomatonConfig
validateArgs ruleVal startVal numLinesVal widthVal moveVal
    | ruleVal < 0 || ruleVal > 255              = Left "Invalid rule number"
    | startVal < 0                              = Left "Start must be positive"
    | maybe False (< 0) numLinesVal             =
            Left "Lines must be non-negative"
    | widthVal < 0                              =
            Left "Width must be non-negative"
    | otherwise                                 = Right $
        AutomatonConfig ruleVal startVal numLinesVal widthVal moveVal

-- Handle unknown arguments
checkUnknownArgs :: [String] -> Either String ()
checkUnknownArgs args =
    let knownFlags = ["--rule", "--start", "--lines", "--window", "--move"]
        unknownArgs = filter
            (\arg -> take 2 arg == "--" && arg `notElem` knownFlags) args
    in if null unknownArgs
       then Right ()
       else Left $ "Unknown arguments: " ++ unwords unknownArgs

-- Lookup a flag in the arguments
lookupFlag :: String -> [String] -> Maybe String
lookupFlag flag args = case dropWhile (/= flag) args of
    (_:value:_) -> Just value
    _           -> Nothing

-- Read a value from a string
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(val, "")] -> Just val
    _           -> Nothing
