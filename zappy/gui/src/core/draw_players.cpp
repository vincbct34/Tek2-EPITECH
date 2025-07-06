/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** draw_players
*/

#include "core/Game.hpp"
#include <cmath>

int getYposFromOrientation(int orientation) {
    switch (orientation) {
        case 1: return 18 * 4; // North
        case 2: return 18 * 7; // East
        case 3: return 18 * 6; // South
        case 4: return 18 * 5; // West
        default: return 0;
    }
}

Rectangle Game::getPlayerRectangle(Player &player) {
    if (elevations->getElevation()) {
        for (const auto &id : elevations->getIds()) {
            if (id == player.getId()) {
                if (player.getStartAnimationTime() < 0) {
                    player.setStartAnimationTime(CustomRayLib::getTime());
                    player.setSpriteIndex(0);
                }
                if (player.getSpriteIndex() < 6) {
                    double currentTime = CustomRayLib::getTime();
                    if (currentTime - player.getStartAnimationTime() > 0.3) {
                        player.setSpriteIndex(player.getSpriteIndex() + 1);
                        player.setStartAnimationTime(CustomRayLib::getTime());
                    }
                    int frameX = 18 * player.getSpriteIndex();
                    int frameY = 18 * 2;
                    return { (float)frameX, (float)frameY, 18.0f, 18.0f };
                }
            }
        }
    }
    int frameX = 18 * player.getSpriteIndex();
    int frameY = getYposFromOrientation(player.getOrientation());
    return { (float)frameX, (float)frameY, 18.0f, 18.0f };
}

void Game::drawPlayersInTeams(int tileX, int tileY, const Rectangle &tileRect, int componentsSize) {
    int count = componentsSize;
    int i = count;
    
    // Count players and eggs on the tile for grid calculation
    for (const auto &team : teams) {
        if (team->getPlayers().empty()) continue;
        for (const auto &player : team->getPlayers()) {
            if (player->getPosX() == tileX && player->getPosY() == tileY)
                count += 1;
            if (player->getEggs().empty()) continue;
            for (const auto &egg : player->getEggs()) {
                if (egg->getPosX() == tileX && egg->getPosY() == tileY) {
                    count += 1;
                }
            }
        }
    }

    int gridSize = std::ceil(std::sqrt(count));
    float cellWidth = tileRect.width / gridSize;
    float cellHeight = tileRect.height / gridSize;

    for (const auto &team : teams) {
        if (team->getPlayers().empty()) continue;
        for (const auto &player : team->getPlayers()) {
            if (player->getPosX() == tileX && player->getPosY() == tileY) {
                Rectangle sourceRect = getPlayerRectangle(*player);
                int row = i / gridSize;
                int col = i % gridSize;
                float x = tileRect.x + col * cellWidth;
                float y = tileRect.y + row * cellHeight;

                Texture2D& playerTexture = player->getTexture();
                CustomRayLib::drawTexturePro(
                    playerTexture,
                    sourceRect,
                    {x, y, cellWidth, cellHeight},
                    { 0, 0 },
                    0.0f,
                    WHITE
                );
                CustomRayLib::drawRectangleLines(
                    x, y, cellWidth - 1, cellHeight - 1, team->getColor()
                );
                i++;
            }
            for (const auto &egg : player->getEggs()) {
                if (egg->getPosX() == tileX && egg->getPosY() == tileY) {
                    int eggWidth = 221;
                    int eggHeight = 260;

                    if (egg->isHatching()) {
                        double currentTime = CustomRayLib::getTime();
                        if (currentTime - egg->getStartHatchTime() > 1.0) {
                            int spriteIndex = egg->getSpriteIndex();
                            if (spriteIndex < 5) {
                                egg->setSpriteIndex(spriteIndex + 1);
                                egg->setStartHatchTime(currentTime);
                            } else
                                egg->setHatching(false);
                        }
                    }
                    int frameX = eggWidth * egg->getSpriteIndex();
                    Rectangle sourceRect = { (float)frameX, (float)0, (float)eggWidth, (float)eggHeight };
                    int row = i / gridSize;
                    int col = i % gridSize;
                    float x = tileRect.x + col * cellWidth;
                    float y = tileRect.y + row * cellHeight;
                    std::cout << "[DEBUG] Drawing egg for player " << player->getId() << " at (" << x << ", " << y << ") in team " << team->getName() << std::endl;
                    CustomRayLib::drawTexturePro(
                        egg->getTexture(),
                        sourceRect,
                        { x, y, cellWidth, cellHeight },
                        { 0, 0 },
                        0.0f,
                        WHITE
                    );
                    i++;
                }
            }
        }
    }
}
