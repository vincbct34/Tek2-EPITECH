/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** Component
*/

#pragma once

#include "utils/CustomRayLib.hpp"
#include <string>
#include <vector>
#include <iostream>
#include <algorithm>

enum class ComponentType {
    NONE,
    FOOD,
    LINEMATE,
    DERAUMERE,
    SIBUR,
    MENDIANE,
    PHIRAS,
    THYSTAME,
    EGG,
    ELEVATION,
    PLAYER,
};

class Component {
    public:
        Component(ComponentType type, int quantity, int sizeX, int sizeY);
        ~Component() = default;
        ComponentType getType() const { return type; }
        int getQuantity() const { return quantity; }
        void setQuantity(int newQuantity) { quantity = newQuantity; }
        Texture2D& getTexture();
        bool isTextureLoaded() const { return textureLoaded; }
        int getId() const { return static_cast<int>(type); }

    private:
        ComponentType type;
        int quantity;
        int sizeX, sizeY;
        Texture2D texture = {};
        bool textureLoaded = false;
        void loadTexture();
};