/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** snakeEnd
*/

#include "Snake.hpp"

namespace Arcade {

    void Snake::displayEndScreen() {
        auto overlay = std::make_unique<ARectangle>(
            std::make_pair(200.0, 200.0),
            std::make_pair(0.0, 0.0),
            std::make_tuple(0, 0, 0),
            3
        );
        _elements.push_back(std::move(overlay));

        auto gameOverText = std::make_unique<AText>(
            std::make_pair(0, 0),
            std::make_pair(50.0, 40.0),
            "GAME OVER",
            true,
            5
        );
        gameOverText->setColor({255, 0, 0});
        _elements.push_back(std::move(gameOverText));

        auto scoreText = std::make_unique<AText>(
            std::make_pair(0, 0),
            std::make_pair(50.0, 50.0),
            "Score: " + std::to_string(_score),
            false,
            6
        );
        scoreText->setColor({255, 255, 255});
        _elements.push_back(std::move(scoreText));

        auto exitText = std::make_unique<AText>(
            std::make_pair(0, 0),
            std::make_pair(50.0, 60.0),
            "Press ENTER to return to menu",
            false,
            7
        );
        exitText->setColor({200, 200, 200});
        _elements.push_back(std::move(exitText));
    }

}
