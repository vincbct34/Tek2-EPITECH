/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** draw_elevations
*/

#include "core/Game.hpp"
#include <cmath>

void Game::drawElevations(int tileX, int tileY, const Rectangle &tileRect) {
    if (elevations->getElevationSuccess() && elevations->getPosX() == tileX && elevations->getPosY() == tileY) {
        std::cout << "[DEBUG] Drawing elevation at (" << tileX << ", " << tileY << ")" << std::endl;

        float zoomFactor = 1.5f;
        float cellWidth = tileRect.width * zoomFactor;
        float cellHeight = tileRect.height * zoomFactor;
        float offsetX = (cellWidth - tileRect.width) / 2.0f;
        float offsetY = (cellHeight - tileRect.height) / 2.0f;
        int elevWidth = 1524;
        int elevHeight = 1058;
        double elapsedTime = CustomRayLib::getTime() - elevations->getStartTime();

        if (elapsedTime > 0.1) {
            int spriteIndex = elevations->getSpriteIndex();
            if (spriteIndex < 9) {
                elevations->setSpriteIndex(spriteIndex + 1);
                elevations->setStartTime(CustomRayLib::getTime());
            } else {
                elevations->setSpriteIndex(0);
                elevations->setElevationSuccess(false);
            }
        }
        std::cout << "[DEBUG] Elevation elapsedTime: " << elapsedTime << std::endl;

        int frameX = 1524 * elevations->getSpriteIndex();
        int frameY = 0;
        Rectangle sourceRect = { (float)frameX, (float)frameY, (float)elevWidth, (float)elevHeight };
        Rectangle destRect = { 
            tileRect.x - offsetX, 
            tileRect.y - offsetY, 
            cellWidth, 
            cellHeight 
        };
        CustomRayLib::drawTexturePro(
            elevations->getTexture(),
            sourceRect,
            destRect,
            { 0, 0 },
            0.0f,
            WHITE
        );
    }
}