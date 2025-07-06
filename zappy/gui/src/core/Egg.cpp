/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** Egg
*/

#include "core/Egg.hpp"
#include <iostream>

void Egg::loadTexture() {
    if (textureLoaded) return;
    
    try {
        Image image;
        CustomRayLib::loadImage("./gui/assets/egg.png", image);
        if (image.data == nullptr) {
            std::cerr << "[ERROR] Failed to load image for egg with id: " << id << std::endl;
            return;
        }
        
        CustomRayLib::loadTextureFromImage(image, texture);
        CustomRayLib::unloadImage(image);
        
        if (texture.id == 0) {
            std::cerr << "[ERROR] Failed to load texture for egg with id: " << id << std::endl;
            return;
        }
        
        textureLoaded = true;
    } catch (const std::exception& e) {
        std::cerr << "[ERROR] Exception loading texture for egg: " << e.what() << std::endl;
    }
}

Egg::Egg(int id, int posX, int posY) : id(id), posX(posX), posY(posY), textureLoaded(false) {
    // Texture sera chargée lors du premier appel à getTexture()
}

Egg::~Egg() {}
