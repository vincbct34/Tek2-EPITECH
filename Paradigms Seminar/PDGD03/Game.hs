{-
-- EPITECH PROJECT, 2025
-- Game.hs
-- File description:
-- Paradigms Seminar - Third day of Haskell pool
-}

module Game where

-- The data structure of the items
data Item = Sword | Bow | MagicWand
    deriving (Eq)

-- The instance of Show for the items
instance Show Item where
    show Sword = "sword"
    show Bow = "bow"
    show MagicWand = "magic wand"

-- The data structure of the mobs
data Mob = Mummy | Skeleton Item | Witch (Maybe Item)
    deriving (Eq)

-- The mobs that are created
createMummy :: Mob
createMummy = Mummy

createArcher :: Mob
createArcher = Skeleton Bow

createKnight :: Mob
createKnight = Skeleton Sword

createWitch :: Mob
createWitch = Witch Nothing

createSorceress :: Mob
createSorceress = Witch (Just MagicWand)

-- The function to create the mobs
create :: String -> Maybe Mob
create string = case string of
    "mummy" -> Just createMummy
    "doomed archer" -> Just createArcher
    "dead knight" -> Just createKnight
    "witch" -> Just createWitch
    "sorceress" -> Just createSorceress
    _ -> Nothing

-- The function to equip the mobs with items
equip :: Item -> Mob -> Maybe Mob
equip item mob = case mob of
    Mummy -> Nothing
    Skeleton _ -> Just (Skeleton item)
    Witch _ -> Just (Witch (Just item))

-- The instance of Show for the mobs
instance Show Mob where
    show Mummy = "mummy"
    show (Skeleton Bow) = "doomed archer"
    show (Skeleton Sword) = "dead knight"
    show (Skeleton item) = "skeleton holding a " ++ show item
    show (Witch Nothing) = "witch"
    show (Witch (Just MagicWand)) = "sorceress"
    show (Witch (Just item)) = "witch holding a " ++ show item

-- The class HasItem with the functions getItem and hasItem
class HasItem a where
    getItem :: a -> Maybe Item
    hasItem :: a -> Bool
    hasItem item = case getItem item of
        Just _ -> True
        Nothing -> False

-- The instance of HasItem for the items
instance HasItem Mob where
    hasItem Mummy = False
    hasItem (Witch Nothing) = False
    hasItem _ = True

    getItem Mummy = Nothing
    getItem (Skeleton item) = Just item
    getItem (Witch (Just item)) = Just item
    getItem _ = Nothing
