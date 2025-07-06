/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** snake
*/

#pragma once

#include "IGame.hpp"

namespace Arcade {
    class Snake : public IGame {
    public:
        Snake();
        ~Snake() override = default;

        void init(std::vector<std::string> gamesLib, std::vector<std::string> graphicsLibs, int sizeX, int sizeY, std::unordered_map<std::string, std::array<std::pair<std::string, int>, 3>>) override;
        void getInput(Event event) override;
        void displayEndScreen() override;

        const std::vector<std::unique_ptr<IEntity>> &getElement() const override;
        int updateGame(Event event) override;

        std::string getChosenGraphical() const override;
        std::string getChosenGame() const override;
        std::string getGameName() const override;

    private:
        void updateDirection(const Event &event);
        std::vector<std::pair<double, double>> getOldPositions(std::vector<std::pair<double, double>> oldPositions);
        void moveSnake();
        void updateSegmentsPositions(std::vector<std::pair<double, double>> oldPositions);
        void checkAppleCollision();
        void checkSelfCollision();

        std::vector<std::unique_ptr<IEntity>> _elements;

        std::string _chosenGraphical;
        std::string _chosenGame = "lib/arcade_snake.so";

        int _direction = -1;
        int _score = 0;
        int _posX = 50;
        int _posY = 50;

        std::pair<int, int> _windowSize;
    };

    extern "C" Snake *createGame();
}
