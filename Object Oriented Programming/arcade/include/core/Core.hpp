/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** Core
*/

#pragma once

#include "DlEncapsulation.hpp"
#include "IGraphical.hpp"
#include "Entity.hpp"
#include "IGame.hpp"
#include "Event.hpp"

#include <unordered_map>
#include <unordered_set>
#include <filesystem>
#include <algorithm>
#include <iostream>
#include <fstream>
#include <dlfcn.h>
#include <memory>
#include <string>
#include <vector>
#include <tuple>
#include <array>

namespace Arcade {

    std::vector<std::string> getGraphicalsLibraries(const std::string &graphicslibPath);
    std::vector<std::string> getGamesLibraries(const std::string &gameslibPath);
    bool isArgValid(const std::vector<std::string> &graphicsLibs, const std::vector<std::string> &gamesLibs, char *argv[]);
    void startArcade(char **argv);

    class Core {
    public:
        Core() = default;
        ~Core();

        bool loadGame(const std::string &gamePath);
        bool loadGraphics(const std::string &graphicsPath);
        void changeGraphical(const std::vector<std::string> &graphicsLibs);
        void run(const std::vector<std::string> &gamesLib, const std::vector<std::string> &graphicsLibs);
        void saveScores(const std::string &gameName, const std::string &playerName, int score);
        std::unordered_map<std::string, std::array<std::pair<std::string, int>, 3>> getMaxScores();

    private:
        void reloadGame(const std::string &gamePath, const std::vector<std::string> &gamesLib, const std::vector<std::string> &graphicsLibs);
        void handleEndGame(const std::vector<std::string> &gamesLib, const std::vector<std::string> &graphicsLibs, int score, int currentGameIndex);
        void switchToNextGame(const std::vector<std::string> &gamesLib, const std::vector<std::string> &graphicsLibs, int &currentGameIndex);
        void initializeGameAndGraphics(const std::string &gamePath, const std::string &graphicsPath, const std::vector<std::string> &gamesLib, const std::vector<std::string> &graphicsLibs);

        void *_gameLibHandle = nullptr;
        void *_graphLibHandle = nullptr;
        std::string _playerName = "Player";
        std::unique_ptr<IGame> _game;
        std::unique_ptr<IGraphical> _graphic;
        std::string _graphicLibLoaded;
        std::pair<int, int> _windowSize = {800, 600};
        std::unordered_map<std::string, std::array<std::pair<std::string, int>, 3>> _maxScores;
        bool _coreIsRunning = false;
    };
}
