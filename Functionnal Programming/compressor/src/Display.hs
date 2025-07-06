{-
-- EPITECH PROJECT, 2025
-- ImageCompressor
-- File description:
-- Display
-}

module Display (displayLogic) where

import KMeans (randomizedCentroids, kMeansLoop)
import Utilities (Color, Pixel, Cluster)

-- Affichage + logique du K-Means
displayLogic :: Int -> Float -> Cluster -> IO ()
displayLogic n limit pixels = do
    centroids <- randomizedCentroids n pixels
    let finalClusters = kMeansLoop limit centroids pixels
    mapM_ printCluster finalClusters

-- Affiche un cluster au format demandé
printCluster :: (Color, Cluster) -> IO ()
printCluster (color, pixels) =
    putStrLn "--"
    >> printColor color
    >> putStrLn "-"
    >> mapM_ printPixel pixels

-- Affiche une couleur au format demandé
printColor :: Color -> IO ()
printColor (r, g, b) = 
    putStrLn $ "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

-- Affiche un pixel au format demandé
printPixel :: Pixel -> IO ()
printPixel ((x, y), (r, g, b)) =
    putStrLn $ "(" ++ show x ++ "," ++ show y ++ ") "
             ++ "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"
