/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** Egg
*/

#pragma once

#include "core/Game.hpp"
#include "utils/CustomRayLib.hpp"
#include "core/Component.hpp"
#include <string>
#include <memory>

class Egg {
    public:
        Egg(int id, int posX, int posY);
        ~Egg();

        int getPosX() const { return posX; }
        int getPosY() const { return posY; }
        int getId() const { return id; }
        const Texture2D &getTexture() const { 
            const_cast<Egg*>(this)->loadTexture();
            return texture; 
        }
        const Image &getImage() const { return image; }
        int getSpriteIndex() const { return spriteIndex; }
        bool isHatching() const { return _isHatching; }
        void setPosX(int x) { posX = x; }
        void setPosY(int y) { posY = y; }
        void setTexture(const Texture2D &tex) { texture = tex; }
        void loadTexture();
        void setImage(const Image &img) { image = img; }
        void setHatching(bool hatching) { _isHatching = hatching; }
        void setSpriteIndex(int index) { spriteIndex = index; }
        void setStartHatchTime(double time) { startHatchTime = time; }
        double getStartHatchTime() const { return startHatchTime; }
    private:
        int id;
        int posX;
        int posY;
        Image image;
        Texture2D texture = {};
        Rectangle rectangleEgg;
        bool _isHatching = false;
        double startHatchTime = 0.0;
        int spriteIndex = 0;
        bool textureLoaded = false;
};
