/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** Tile
*/

#pragma once

#include "utils/CustomRayLib.hpp"
#include "core/Component.hpp"
#include <string>
#include <vector>
#include <iostream>
#include <algorithm>
#include <memory>

class Tile {
    public:
        Tile(int x, int y, int gridWidth, int gridHeight, int windowWidth, int windowHeight);
        ~Tile();

        /**
         * @brief Gets the rectangle representing the tile.
         *
         * @return The rectangle of the tile.
         */
        Rectangle getRectangle() const { return rectangle; };

        /**
         * @brief Gets the x coordinate of the tile in the grid.
         *
         * @return The x coordinate of the tile.
         */
        int getX() const { return x; } // Get the x coordinate of the tile
        /**
         * @brief Gets the y coordinate of the tile in the grid.
         *
         * @return The y coordinate of the tile.
         */
        int getY() const { return y; } // Get the y coordinate of the tile
        /**
         * @brief Gets the size of the tile in pixels.
         *
         * @return The size of the tile in pixels.
         */
        int getSizeX() const { return sizeX; }
        /**
         * @brief Gets the size of the tile in pixels.
         *
         * @return The size of the tile in pixels.
         */
        int getSizeY() const { return sizeY; }

        int getNumberOfComponents() const { return components.size(); }


        /**
         * @brief Add a component to the tile.
         * @param component The component to add.
         */
        void addComponent(std::shared_ptr<Component> component) { components.push_back(component); }
        /**
         * @brief Get the components present on the tile.
         * @return A vector of components present on the tile.
         */
        const std::vector<std::shared_ptr<Component>>& getComponents() const { return components; }
        
        /**
         * @brief Get the components present on the tile (non-const version).
         * @return A vector of components present on the tile.
         */
        std::vector<std::shared_ptr<Component>>& getComponents() { return components; }
        
        /**
         * @brief Remove a component from the tile.
         * @param component The component to remove.
         */
        void removeComponent(const std::shared_ptr<Component>& component) {
            components.erase(std::remove_if(components.begin(), components.end(),
                [&component](const std::shared_ptr<Component>& comp) {
                    return comp.get() == component.get();
                }), components.end());
        }

        void addResource(int resourceId);
    private:
        Rectangle rectangle;
        int x, y;
        int sizeX, sizeY;
        std::vector<std::shared_ptr<Component>> components;
};
