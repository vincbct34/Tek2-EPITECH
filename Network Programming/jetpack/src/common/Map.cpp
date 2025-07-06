/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Map
*/

#include "common/Map.hpp"

bool Map::loadFromFile(const std::string& path) {
    std::ifstream file(path); // Open the file for reading
    std::string line;

    // Check if the file was successfully opened
    if (!file.is_open())
        return false;

    mapData.clear(); // Clear any existing map data

    // Read each line from the file and store it in the map data
    while (std::getline(file, line))
        mapData.push_back(line);

    return true; // Return true if the file was read successfully
}

const std::vector<std::string>& Map::getData() const {
    // Return the map data as a constant reference
    return mapData;
}

bool Map::setTile(int x, int y, char tile) {
    // Check if the coordinates are out of bounds
    if (y >= (int)mapData.size() || x >= (int)mapData[y].size())
        return false;

    // Set the tile character at the specified coordinates
    mapData[y][x] = tile;
    return true; // Return true if the tile was set successfully
}

char Map::getTile(int x, int y) const {
    // Check if the coordinates are out of bounds
    if (y >= (int)mapData.size() || x >= (int)mapData[y].size())
        return ' '; // Return a space character if out of bounds

    // Return the tile character at the specified coordinates
    return mapData[y][x];
}

size_t Map::getWidth() const {
    // Return the width of the first row, or 0 if the map is empty
    return mapData.empty() ? 0 : mapData[0].size();
}

size_t Map::getHeight() const {
    // Return the number of rows in the map
    return mapData.size();
}
