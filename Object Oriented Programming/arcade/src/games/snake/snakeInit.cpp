/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** snakeInit
*/

#include "Snake.hpp"

namespace Arcade {

    void Snake::init(std::vector<std::string> gamesLib, std::vector<std::string> graphicsLibs, int sizeX, int sizeY, std::unordered_map<std::string, std::array<std::pair<std::string, int>, 3>>) {
        (void)gamesLib;
        (void)graphicsLibs;

        _windowSize = {sizeX, sizeY};
        _elements.clear();
        _score = 0;
        _direction = -1;

        auto head = std::make_unique<ARectangle>(std::make_pair(2, 2), std::make_pair(_posX, _posY), std::make_tuple(171, 255, 0), 0);
        _elements.push_back(std::move(head));

        for (int i = 0; i < 3; ++i) {
            auto body = std::make_unique<ARectangle>(std::make_pair(2, 2), std::make_pair(30, 30), std::make_tuple(100, 100, 100), 1);
            _elements.push_back(std::move(body));
        }

        auto apple = std::make_unique<ARectangle>(std::make_pair(2, 2), std::make_pair(20, 50), std::make_tuple(255, 0, 0), 2);
        _elements.push_back(std::move(apple));
    }

}
