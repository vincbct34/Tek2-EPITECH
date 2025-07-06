{-
-- EPITECH PROJECT, 2025
-- Tree.hs
-- File description:
-- Paradigms Seminar - Third day of Haskell pool
-}

module Tree where

data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving (Show)

addInTree :: Ord a => a -> Tree a -> Tree a
addInTree value Empty = Node Empty value Empty
addInTree value (Node left nodeValue right)
    | value < nodeValue = Node (addInTree value left) nodeValue right
    | otherwise         = Node left nodeValue (addInTree value right)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap func (Node left value right) =
        Node (fmap func left) (func value) (fmap func right)

listToTree :: Ord a => [a] -> Tree a
listToTree = foldr addInTree Empty

treeToList :: Ord a => Tree a -> [a]
treeToList Empty = []
treeToList (Node left value right)
    = treeToList left ++ [value] ++ treeToList right

treeSort :: Ord a => [a] -> [a]
treeSort list = treeToList (listToTree list)

instance Foldable Tree where
    foldr _ acc Empty = acc
    foldr func acc (Node left value right) =
        foldr func (func value (foldr func acc right)) left
