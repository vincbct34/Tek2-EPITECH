/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Map
*/

#pragma once

#include <fstream>
#include <string>
#include <vector>

class Map {
public:
    /**
     * @brief Constructs a Map object.
     */
    Map() = default;
    /**
     * @brief Destroys the Map object.
     */
    ~Map() = default;
    /**
     * @brief Loads a map from a file.
     *
     * @param path The file path to the map.
     * @return true if the map was loaded successfully, false otherwise.
     */
    bool loadFromFile(const std::string& path);
    
    const std::vector<std::string>& getData() const;
    /**
     * @brief Sets a tile in the map.
     *
     * @param x The x-coordinate of the tile.
     * @param y The y-coordinate of the tile.
     * @param tile The character representing the tile.
     * @return true if the tile was set successfully, false otherwise.
     */
    bool setTile(int x, int y, char tile);
    /**
     * @brief Gets a tile from the map.
     *
     * @param x The x-coordinate of the tile.
     * @param y The y-coordinate of the tile.
     * @return The character representing the tile.
     */
    char getTile(int x, int y) const;
    /**
     * @brief Gets the width of the map.
     *
     * @return The width of the map.
     */
    size_t getWidth() const;
    /**
     * @brief Gets the height of the map.
     *
     * @return The height of the map.
     */
    size_t getHeight() const;

private:
    std::vector<std::string> mapData; // The map data
};
