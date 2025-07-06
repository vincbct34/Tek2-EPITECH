/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** Tile
*/

#include "Tile.hpp"

Tile::Tile(int x, int y, int gridWidth, int gridHeight, int windowWidth, int windowHeight) {
    float tileWidth = static_cast<float>(windowWidth) / gridWidth;
    float tileHeight = static_cast<float>(windowHeight) / gridHeight;

    rectangle = {x * tileWidth, y * tileHeight, tileWidth, tileHeight};
    this->x = x;
    this->y = y;
    this->sizeX = tileWidth;
    this->sizeY = tileHeight;
    std::cout << "[DEBUG] Tile size: " << sizeX << "x" << sizeY << std::endl;
}

Tile::~Tile() {
    std::cout << "[DEBUG] Tile at (" << x << ", " << y << ") destroyed." << std::endl;
    components.clear();
}

void Tile::addResource(int resourceId) {
    ComponentType type;

    switch (resourceId) {
        case 0: type = ComponentType::FOOD; break;
        case 1: type = ComponentType::LINEMATE; break;
        case 2: type = ComponentType::DERAUMERE; break;
        case 3: type = ComponentType::SIBUR; break;
        case 4: type = ComponentType::MENDIANE; break;
        case 5: type = ComponentType::PHIRAS; break;
        case 6: type = ComponentType::THYSTAME; break;
        default:
            std::cerr << "[ERROR] Unknown resource ID: " << resourceId << std::endl;
            return;
    }
    for (auto &component : components) {
        if (component->getType() == type) {
            // Augmenter la quantité du composant existant
            component->setQuantity(component->getQuantity() + 1);
            std::cout << "[DEBUG] Resource quantity increased: Type=" << (int)type << ", New quantity=" << component->getQuantity() << std::endl;
            return;
        }
    }
    auto newComponent = std::make_shared<Component>(type, 1, sizeX, sizeY);
    components.push_back(newComponent);
    std::cout << "[DEBUG] New resource added to tile: Type=" << (int)type << ", ResourceID=" << resourceId << std::endl;
}
