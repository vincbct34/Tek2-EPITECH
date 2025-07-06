{-
-- EPITECH PROJECT, 2025
-- Process.hs
-- File description:
-- Paradigms Seminar - Rush of Haskell pool
-}

module Process where

-- Import from the Haskell libraries
import System.Exit (exitWith, ExitCode(ExitFailure))

-- Operations
sa :: ([Int], [Int]) -> ([Int], [Int])
sa ([], l_b) = ([], l_b)
sa (x:y:xs, l_b) = (y:x:xs, l_b)

sb :: ([Int], [Int]) -> ([Int], [Int])
sb (l_a, []) = (l_a, [])
sb (l_a, x:y:xs) = (l_a, y:x:xs)

sc :: ([Int], [Int]) -> ([Int], [Int])
sc = sa . sb

pa :: ([Int], [Int]) -> ([Int], [Int])
pa (l_a, []) = (l_a, [])
pa (l_a, x:xs) = (x:l_a, xs)

pb :: ([Int], [Int]) -> ([Int], [Int])
pb ([], l_b) = ([], l_b)
pb (x:xs, l_b) = (xs, x:l_b)

ra :: ([Int], [Int]) -> ([Int], [Int])
ra ([], l_b) = ([], l_b)
ra (x:xs, l_b) = (xs ++ [x], l_b)

rb :: ([Int], [Int]) -> ([Int], [Int])
rb (l_a, []) = (l_a, [])
rb (l_a, x:xs) = (l_a, xs ++ [x])

rr :: ([Int], [Int]) -> ([Int], [Int])
rr = ra . rb

rra :: ([Int], [Int]) -> ([Int], [Int])
rra ([], l_b) = ([], l_b)
rra (xs, l_b) = (last xs : init xs, l_b)

rrb :: ([Int], [Int]) -> ([Int], [Int])
rrb (l_a, []) = (l_a, [])
rrb (l_a, xs) = (l_a, last xs : init xs)

rrr :: ([Int], [Int]) -> ([Int], [Int])
rrr = rra . rrb

-- Map operations
applyOperation :: String -> ([Int], [Int]) -> IO ([Int], [Int])
applyOperation "sa"  = return . sa
applyOperation "sb"  = return . sb
applyOperation "sc"  = return . sc
applyOperation "pa"  = return . pa
applyOperation "pb"  = return . pb
applyOperation "ra"  = return . ra
applyOperation "rb"  = return . rb
applyOperation "rr"  = return . rr
applyOperation "rra" = return . rra
applyOperation "rrb" = return . rrb
applyOperation "rrr" = return . rrr
applyOperation _     = \_ -> exitWith (ExitFailure 84)