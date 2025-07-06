/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** snake
*/

#pragma once

#pragma once

#include "IGame.hpp"
#include <vector>
#include <memory>
#include <string>
#include <unordered_map>
#include <array>

namespace Arcade {

    class Centipede : public IGame {
    public:
        Centipede();
        ~Centipede() = default;

        void init(std::vector<std::string> gamesLib,
                  std::vector<std::string> graphicsLibs,
                  int sizeX, int sizeY,
                  std::unordered_map<std::string, std::array<std::pair<std::string, int>, 3>>) override;

        void getInput(Event event) override;
        void displayEndScreen() override;

        const std::vector<std::unique_ptr<IEntity>> &getElement() const override;
        int updateGame(Event event) override;

        std::string getChosenGraphical() const override;
        std::string getChosenGame() const override;
        std::string getGameName() const override;

    private:
        void spawnCentipede();
        void handlePlayerMovement(Event event);
        void handleShooting(Event event);
        void updateProjectiles();
        void updateCentipedes();
        void cleanupOutOfBoundsCentipedes();
        void checkGameOver();

        std::vector<std::unique_ptr<IEntity>> _elements;

        std::pair<int, int> _windowSize;
        std::string _chosenGraphical;
        std::string _chosenGame = "lib/arcade_centipede.so";

        int _score = 0;
        int _nbCentipede = 0;
        int _centipedeCount = 0;
        const int _maxCentipedes = 20;
    };

}