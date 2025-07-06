/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** centipedeInit
*/

#include "Centipede.hpp"

namespace Arcade {

    void Centipede::init(std::vector<std::string> gamesLib,
                         std::vector<std::string> graphicsLibs,
                         int sizeX, int sizeY,
                         std::unordered_map<std::string, std::array<std::pair<std::string, int>, 3>>) {
        (void)gamesLib;
        (void)graphicsLibs;
        _gameState = GameState::RUNNING;
        _windowSize = {sizeX, sizeY};

        auto player = std::make_unique<ARectangle>(
            std::make_pair(1, 4), std::make_pair(50, 90), std::make_tuple(171, 255, 0), 0
        );
        _elements.push_back(std::move(player));

        for (int i = 0; i < (rand() % 40) + 30; i++) {
            auto mushroom = std::make_unique<ARectangle>(
                std::make_pair(2, 3),
                std::make_pair(10 + rand() % 81, 3 + rand() % 70),
                std::make_tuple(100, 100, 100),
                2
            );
            _elements.push_back(std::move(mushroom));
        }

        spawnCentipede();
    }

    void Centipede::spawnCentipede() {
        if (_centipedeCount >= _maxCentipedes)
            return;

        for (int i = 0; i < rand() % 5 + 10; i++) {
            auto segment = std::make_unique<ARectangle>(
                std::make_pair(2, 3),
                std::make_pair(20 + (2 * i), 0),
                std::make_tuple(255, 255, 10),
                3
            );
            _elements.push_back(std::move(segment));
            _nbCentipede++;
        }
        _centipedeCount++;
    }

}
