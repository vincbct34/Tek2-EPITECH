/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** Core
*/

#include "Core.hpp"
#include <dlfcn.h>

namespace Arcade {

    Core::~Core() {
        _game.reset();
        _graphic.reset();
        if (_gameLibHandle) dlclose(_gameLibHandle);
        if (_graphLibHandle) dlclose(_graphLibHandle);
    }

    void Core::initializeGameAndGraphics(const std::string &gamePath, const std::string &graphicsPath, const std::vector<std::string> &gamesLib, const std::vector<std::string> &graphicsLibs) {
        _graphic->stop();
        loadGraphics(graphicsPath);
        _graphic->init();
        loadGame(gamePath);
        _game->init(gamesLib, graphicsLibs, _windowSize.first, _windowSize.second, _maxScores);
    }

    void Core::reloadGame(const std::string &gamePath, const std::vector<std::string> &gamesLib, const std::vector<std::string> &graphicsLibs) {
        _game.reset();
        initializeGameAndGraphics(gamePath, _graphicLibLoaded, gamesLib, graphicsLibs);
    }

    void Core::handleEndGame(const std::vector<std::string> &gamesLib, const std::vector<std::string> &graphicsLibs, int score, int currentGameIndex) {
        if (gamesLib[currentGameIndex] != gamesLib[0])
            saveScores(gamesLib[currentGameIndex], _playerName, score);
        reloadGame(gamesLib[0], gamesLib, graphicsLibs);
    }

    void Core::switchToNextGame(const std::vector<std::string> &gamesLib, const std::vector<std::string> &graphicsLibs, int &currentGameIndex) {
        currentGameIndex = (currentGameIndex + 1) % gamesLib.size();
        if (currentGameIndex == 0)
            currentGameIndex = 1;
        reloadGame(gamesLib[currentGameIndex], gamesLib, graphicsLibs);
    }

    void Core::run(const std::vector<std::string> &gamesLib, const std::vector<std::string> &graphicsLibs) {
        if (!_graphic || !_game) {
            std::cerr << "[ERROR] Missing game or graphics library." << std::endl;
            return;
        }

        _maxScores = getMaxScores();
        _graphic->init();
        _playerName = _graphic->getPlayerName();
        _game->init(gamesLib, graphicsLibs, _windowSize.first, _windowSize.second, _maxScores);

        Event event;
        int currentGameIndex = 0;
        int score = 0;
        _coreIsRunning = true;

        while (_coreIsRunning) {
            const auto &elements = _game->getElement();
            _graphic->clearScreen();
            _graphic->drawElements(elements);
            _graphic->refresh();
            event = _graphic->getInput(elements);

            for (size_t i = 0; i < gamesLib.size(); ++i) {
                if (gamesLib[i] == _game->getGameName()) {
                    currentGameIndex = static_cast<int>(i);
                    break;
                }
            }

            switch (event.getType()) {
                case Event::TypeEvent::SPACE_KEY:
                    changeGraphical(graphicsLibs);
                    continue;
                case Event::TypeEvent::TAB_KEY:
                    if (gamesLib[currentGameIndex] != gamesLib[0])
                        reloadGame(gamesLib[currentGameIndex], gamesLib, graphicsLibs);
                    continue;
                case Event::TypeEvent::SHIFT_KEY:
                    if (gamesLib[currentGameIndex] != gamesLib[0])
                        switchToNextGame(gamesLib, graphicsLibs, currentGameIndex);
                    continue;
                case Event::TypeEvent::BACKSPACE:
                    if (gamesLib[currentGameIndex] != gamesLib[0])
                        reloadGame(gamesLib[0], gamesLib, graphicsLibs);
                    continue;
                case Event::TypeEvent::ESCAPE_KEY:
                    _coreIsRunning = false;
                    break;
                default:
                    break;
            }

            _game->getInput(event);
            if (_graphic->canUpdate())
                score = _game->updateGame(event);

            if (_game->getGameState() == IGame::GameState::STOPPED) {
                handleEndGame(gamesLib, graphicsLibs, score, currentGameIndex);
                continue;
            }

            if (_game->sendLibraryChangeSignal()) {
                std::string chosenGame = _game->getChosenGame();
                std::string chosenGraphical = _game->getChosenGraphical();
                reloadGame("lib/" + chosenGame + ".so", gamesLib, graphicsLibs);
                loadGraphics("lib/" + chosenGraphical + ".so");
                _graphic->init();
            }
        }

        _graphic->stop();
    }

    std::unordered_map<std::string, std::array<std::pair<std::string, int>, 3>> Core::getMaxScores() {
        std::unordered_map<std::string, std::array<std::pair<std::string, int>, 3>> scores;
        std::ifstream file("max_scores.txt");
        if (!file.is_open()) {
            std::cerr << "[ERROR] Cannot open max_scores.txt" << std::endl;
            return scores;
        }

        std::string line;
        while (std::getline(file, line)) {
            std::istringstream iss(line);
            std::string game;
            iss >> game;
            std::array<std::pair<std::string, int>, 3> entries = { { {"NoPlayer", 0}, {"NoPlayer", 0}, {"NoPlayer", 0} } };
            for (auto &entry : entries)
                iss >> entry.first >> entry.second;
            scores[game] = entries;
        }

        return scores;
    }

    void Core::saveScores(const std::string &gameName, const std::string &playerName, int score) {
        auto &entries = _maxScores[gameName];
        for (size_t i = 0; i < entries.size(); ++i) {
            if (score > entries[i].second) {
                for (size_t j = entries.size() - 1; j > i; --j)
                    entries[j] = entries[j - 1];
                entries[i] = {playerName, score};
                break;
            }
        }

        std::ofstream file("max_scores.txt");
        if (!file.is_open()) {
            std::cerr << "[ERROR] Cannot write max_scores.txt" << std::endl;
            return;
        }

        for (const auto &[game, scores] : _maxScores) {
            file << game;
            for (const auto &[name, val] : scores)
                file << " " << name << " " << val;
            file << "\n";
        }
    }

}
