/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** Menu
*/

#pragma once

#include "IGame.hpp"

#include <unordered_map>
#include <algorithm>
#include <memory>
#include <vector>
#include <string>
#include <array>

namespace Arcade {

    class Menu : public IGame {
    public:
        Menu();
        ~Menu() = default;

        void init(std::vector<std::string> gamesLibs,
                  std::vector<std::string> graphicsLibs,
                  int, int,
                  std::unordered_map<std::string, std::array<std::pair<std::string, int>, 3>>) override;

        void getInput(Event event) override;
        void displayEndScreen() override {}

        const std::vector<std::unique_ptr<IEntity>> &getElement() const override;
        int updateGame(Event) override;

        std::string getChosenGraphical() const override;
        std::string getChosenGame() const override;
        std::string getGameName() const override;

        bool sendLibraryChangeSignal() override;

    private:
        void setupTitle();
        void setupGameOptions(const std::vector<std::string> &gamesLibs);
        void setupGraphicsOptions(const std::vector<std::string> &graphicsLibs);
        void handleEnter();
        void navigateOptions(int direction);
        void displayScores();

        std::vector<std::unique_ptr<IEntity>> _elements;

        std::string _chosenGraphical;
        std::string _chosenGame;

        bool _isGraphicalChosen = false;
        bool _isGameChosen = false;

        std::unordered_map<std::string, std::array<std::pair<std::string, int>, 3>> _scores;
    };

}
