{-
-- EPITECH PROJECT, 2025
-- ImageCompressor
-- File description:
-- KMeans.hs
-}

module KMeans (
    randomizedCentroids,
    kMeansLoop
) where

import Utilities (Color, Cluster)

import System.Random (randomRIO)
import Data.List (minimumBy)
import Data.Ord (comparing)

-- Initialise n couleurs aléatoires à partir des pixels
randomizedCentroids :: Int -> Cluster -> IO [Color]
randomizedCentroids n pixels = do
    let colors = map snd pixels
    indices <- uniqueRandomIndices n (length colors - 1)
    return [colors !! i | i <- indices]

-- Tire n indices aléatoires uniques entre 0 et max
uniqueRandomIndices :: Int -> Int -> IO [Int]
uniqueRandomIndices 0 _      = return []
uniqueRandomIndices n maxVal = go n []
  where
    go 0 numbers = return numbers
    go k numbers = do
        r <- randomRIO (0, maxVal)
        if r `elem` numbers then go k numbers else go (k - 1) (r : numbers)

-- Boucle principale K-Means
kMeansLoop :: Float -> [Color] -> Cluster -> [(Color, Cluster)]
kMeansLoop limit centroids pixels =
    let clusters = assignPixelsToClusters centroids pixels
        newCentroids = updateCentroids clusters
    in if hasConverged limit centroids newCentroids
        then clusters
        else kMeansLoop limit newCentroids pixels

-- Attribue chaque pixel au centroid le plus proche
assignPixelsToClusters :: [Color] -> Cluster -> [(Color, Cluster)]
assignPixelsToClusters centroids pixels =
    let initial = [(c, []) | c <- centroids]
    in foldr assign initial pixels
  where
    assign pixel@(_, col) acc =
      let nearest = minimumBy (comparing (\(c, _) -> colorDistance c col)) acc
      in map (\(c, ps) ->
                if c == fst nearest then (c, pixel : ps) else (c, ps)) acc

-- Calcule la moyenne RGB pour chaque cluster
updateCentroids :: [(Color, Cluster)] -> [Color]
updateCentroids = map (meanColor . snd)

-- Calcule la couleur moyenne d'un cluster
meanColor :: Cluster -> Color
meanColor pixels =
    let (rs, gs, bs) = unzip3 [ (r, g, b) | (_, (r, g, b)) <- pixels ]
        len = max 1 (length pixels)
    in (sum rs `div` len, sum gs `div` len, sum bs `div` len)

-- Vérifie si les centroids ont convergé
hasConverged :: Float -> [Color] -> [Color] -> Bool
hasConverged limit old new =
    all (< limit) $ zipWith colorDistance old new

-- Calcule la distance entre deux couleurs
colorDistance :: Color -> Color -> Float
colorDistance (r1, g1, b1) (r2, g2, b2) =
    sqrt $ fromIntegral $
        (r1 - r2) * (r1 - r2)
      + (g1 - g2) * (g1 - g2)
      + (b1 - b2) * (b1 - b2)
