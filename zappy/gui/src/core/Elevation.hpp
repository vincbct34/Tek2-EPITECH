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

class Elevation {
    public:
        Elevation() = default;
        ~Elevation() = default;

        void initElevation(int x, int y, int level, std::vector<int> ids);
        bool getElevation() const { return inElevationActive; }
        bool getElevationSuccess() const { return isElevationSuccess; }
        int getPosX() const { return posX; }
        int getPosY() const { return posY; }
        const Texture2D &getTexture() const { 
            const_cast<Elevation*>(this)->loadTexture();
            return texture; 
        }
        const Image &getImage() const { return image; }
        int getSpriteIndex() const { return spriteIndex; }
        double getStartTime() const { return startTime; }
        const std::vector<int>& getIds() const { return ids; }
        void setPosX(int x) { posX = x; }
        void setPosY(int y) { posY = y; }
        void setElevationActive(bool active) { inElevationActive = active; }
        void setElevationSuccess(bool success) { isElevationSuccess = success; }
        void setTexture(const Texture2D &tex) { texture = tex; }
        void loadTexture();
        void setImage(const Image &img) { image = img; }
        void setSpriteIndex(int index) { spriteIndex = index; }
        void setStartTime(double time) { startTime = time; }
        void setIds(const std::vector<int> &idsList) { ids = idsList; }
    private:
        bool inElevationActive = false;
        bool isElevationSuccess = false;
        int posX;
        int posY;
        Image image;
        Texture2D texture = {};
        Rectangle rectangle;
        double startTime = 0.0;
        int spriteIndex = 0;
        std::vector<int> ids;
        bool textureLoaded = false;
};
