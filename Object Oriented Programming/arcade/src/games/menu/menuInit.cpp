/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** menuInit
*/

#include "Menu.hpp"

namespace Arcade {

    void Menu::init(std::vector<std::string> gamesLibs,
                    std::vector<std::string> graphicsLibs,
                    int, int,
                    std::unordered_map<std::string, std::array<std::pair<std::string, int>, 3>> scores) {
        _scores = std::move(scores);
        _gameState = IGame::GameState::RUNNING;
        _isGameChosen = false;
        _isGraphicalChosen = false;

        setupTitle();
        setupGameOptions(gamesLibs);
        setupGraphicsOptions(graphicsLibs);
    }

    void Menu::setupTitle() {
        auto title = std::make_unique<AText>(std::pair<double, double>{50, 50}, std::pair<double, double>{50, 10}, "Arcade Menu", false, 0);
        title->setColor({0, 255, 255});
        _elements.push_back(std::move(title));

        auto gameTitle = std::make_unique<AText>(std::pair<double, double>{20, 20}, std::pair<double, double>{30, 30}, "Games:", false, 0);
        gameTitle->setColor({255, 128, 0});
        _elements.push_back(std::move(gameTitle));
    }

    void Menu::setupGameOptions(const std::vector<std::string> &gamesLibs) {
        int padding = 1;
        for (const auto &game : gamesLibs) {
            std::string gameName = game.substr(game.find_last_of('/') + 1, game.find_last_of('.') - game.find_last_of('/') - 1);
            if (gameName != "arcade_menu") {
                bool highlight = padding == 1;
                _elements.push_back(std::make_unique<AText>(std::pair<double, double>{20, 20}, std::pair<double, double>{30, 30 + padding * 10}, gameName, highlight, 1));
                if (highlight) _chosenGame = gameName;
                padding++;
            }
        }
    }

    void Menu::setupGraphicsOptions(const std::vector<std::string> &graphicsLibs) {
        auto graphicsTitle = std::make_unique<AText>(std::pair<double, double>{20, 20}, std::pair<double, double>{70, 30}, "Graphics:", false, 0);
        graphicsTitle->setColor({255, 128, 0});
        _elements.push_back(std::move(graphicsTitle));

        int padding = 1;
        for (const auto &graphics : graphicsLibs) {
            std::string graphicsName = graphics.substr(graphics.find_last_of('/') + 1, graphics.find_last_of('.') - graphics.find_last_of('/') - 1);
            bool highlight = padding == 1;
            _elements.push_back(std::make_unique<AText>(std::pair<double, double>{20, 20}, std::pair<double, double>{70, 30 + padding * 10}, graphicsName, highlight, 2));
            if (highlight) _chosenGraphical = graphicsName;
            padding++;
        }
    }

}