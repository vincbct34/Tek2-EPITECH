/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** Elevation
*/

#include "core/Elevation.hpp"
#include <iostream>

void Elevation::loadTexture() {
    if (textureLoaded) return;
    
    try {
        Image image;
        CustomRayLib::loadImage("./gui/assets/effect_1524x1058.png", image);
        if (image.data == nullptr) {
            std::cerr << "[ERROR] Failed to load image for elevation at (" << posX << ", " << posY << ")" << std::endl;
            return;
        }
        
        CustomRayLib::loadTextureFromImage(image, texture);
        CustomRayLib::unloadImage(image);
        
        if (texture.id == 0) {
            std::cerr << "[ERROR] Failed to load texture for elevation at (" << posX << ", " << posY << ")" << std::endl;
            return;
        }
        
        textureLoaded = true;
    } catch (const std::exception& e) {
        std::cerr << "[ERROR] Exception loading texture for elevation: " << e.what() << std::endl;
    }
}

void Elevation::initElevation(int x, int y, int level, std::vector<int> ids) {    
    setPosX(x);
    setPosY(y);
    setIds(ids);
    setSpriteIndex(0);
    setElevationActive(true);
    textureLoaded = false; // Reset texture loading state
    setStartTime(CustomRayLib::getTime());

    (void)level; // Level pas utilise
}
