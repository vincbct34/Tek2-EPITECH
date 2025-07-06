/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** draw_components
*/

#include "core/Game.hpp"
#include <cmath>

void Game::drawComponents(const std::vector<std::shared_ptr<Component>> &components, const Rectangle &tileRect, int playerCountOnTile) {
    int count = components.size();
    if (count == 0) return;
    int elementsInTile = count + playerCountOnTile;
    int gridSize = std::ceil(std::sqrt(elementsInTile));
    float cellWidth = tileRect.width / gridSize;
    float cellHeight = tileRect.height / gridSize;
  
    for (int i = 0; i < count; ++i) {
        const auto &component = components[i];
        int row = i / gridSize;
        int col = i % gridSize;
        float x = tileRect.x + col * cellWidth;
        float y = tileRect.y + row * cellHeight;
  
        Texture2D& componentTexture = component->getTexture();
        if (componentTexture.id != 0) {
            CustomRayLib::drawTexturePro(
                componentTexture,
                {0, 0, (float)componentTexture.width, (float)componentTexture.height},
                {x, y, cellWidth, cellHeight},
                {0, 0},
                0.0f,
                WHITE
            );
        } else
            CustomRayLib::drawRectangle(x, y, cellWidth, cellHeight, GRAY);
        std::string quantityText = std::to_string(component->getQuantity());
        CustomRayLib::drawText(quantityText.c_str(), x + 4, y + 2, 14, BLACK);
    }
}
