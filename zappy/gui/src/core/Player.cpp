/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Player
*/

#include "core/Player.hpp"

std::string getPlayerImagePath(int teamId) {
    switch (teamId) {
    case 0:
        return "./gui/assets/ped1.png";
    case 1:
        return "./gui/assets/ped2.png";
    case 2:
        return "./gui/assets/ped3.png";
    case 3:
        return "./gui/assets/ped4.png";
    default:
        return "./gui/assets/ped1.png";
    }
}

void Player::loadTexture() {
    if (textureLoaded) return;
    
    try {
        Image image;
        CustomRayLib::loadImage(getPlayerImagePath(teamId).c_str(), image);
        if (image.data == nullptr) {
            std::cerr << "[ERROR] Failed to load image" << std::endl;
            return;
        }
        
        //CustomRayLib::imageResize(image, sizeX, sizeY);
        CustomRayLib::loadTextureFromImage(image, texture);
        CustomRayLib::unloadImage(image);
        
        if (texture.id == 0) {
            std::cerr << "[ERROR] Failed to load texture" << std::endl;
            return;
        }
        
        textureLoaded = true;
    } catch (const std::exception& e) {
        std::cerr << "[ERROR] Exception loading texture: " << e.what() << std::endl;
    }
}

Player::Player(int id, int posX, int posY, int orientation, int level, int teamId)
    : id(id), posX(posX), posY(posY), orientation(orientation), level(level) {
    
    this->textureLoaded = false;
    this->teamId = teamId;
    /*CustomRayLib::loadImage(getPlayerImagePath(teamId).c_str(), image);
    if (image.data == nullptr)
        throw std::runtime_error("Failed to load image for player with id: " + std::to_string(id));
    CustomRayLib::loadTextureFromImage(image, texture);
    if (texture.id == 0)
        throw std::runtime_error("Failed to load texture for player with id: " + std::to_string(id));
    CustomRayLib::unloadImage(image);*/
}

Player::~Player() { return; }
