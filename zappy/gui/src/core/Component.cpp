/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** Component
*/

#include "Component.hpp"

std::string getCompenantPath(ComponentType type) {
    switch (type) {
        case ComponentType::NONE: return "UNKNOWN";
        case ComponentType::FOOD: return "./gui/assets/food.png";
        case ComponentType::LINEMATE: return "./gui/assets/linemate.png";
        case ComponentType::DERAUMERE: return "./gui/assets/deraumere.png";
        case ComponentType::SIBUR: return "./gui/assets/Sibur.png";
        case ComponentType::MENDIANE: return "./gui/assets/mendiane.png";
        case ComponentType::PHIRAS: return "./gui/assets/phiras.png";
        case ComponentType::THYSTAME: return "./gui/assets/thystame.png";
        case ComponentType::EGG: return "./gui/assets/egg.png";
        case ComponentType::ELEVATION: return "./gui/assets/effect_1524x1058.png";
        case ComponentType::PLAYER: return "./gui/assets/ped1.png";
        default: return "UNKNOWN";
    }
}

Component::Component(ComponentType type, int quantity, int sizeX, int sizeY) {
    this->type = type;
    this->quantity = quantity;
    this->sizeX = sizeX;
    this->sizeY = sizeY;
    this->textureLoaded = false;
}

void Component::loadTexture() {
    if (textureLoaded) return;
    try {
        Image image;
        CustomRayLib::loadImage(getCompenantPath(type).c_str(), image);
        if (image.data == nullptr) {
            std::cerr << "[ERROR] Failed to load image for component type: " << static_cast<int>(type) << std::endl;
            return;
        }
        
        CustomRayLib::imageResize(image, sizeX, sizeY);
        CustomRayLib::loadTextureFromImage(image, texture);
        CustomRayLib::unloadImage(image);
        
        if (texture.id == 0) {
            std::cerr << "[ERROR] Failed to load texture for component type: " << static_cast<int>(type) << std::endl;
            return;
        }
        textureLoaded = true;
    } catch (const std::exception& e) {
        std::cerr << "[ERROR] Exception loading texture for component type " << static_cast<int>(type) << ": " << e.what() << std::endl;
    }
}

Texture2D& Component::getTexture() {
    if (!textureLoaded) {
        loadTexture();
    }
    return texture;
}
