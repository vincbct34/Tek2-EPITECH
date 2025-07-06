/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** loader
*/

#include "Core.hpp"
#include "DlEncapsulation.hpp"

namespace Arcade {

    using CreateGameFunc = IGame *(*)();
    using CreateGraphicsFunc = IGraphical *(*)();

    bool Core::loadGame(const std::string &gamePath) {
        _game.reset();
        if (_gameLibHandle)
            DlEncapsulation::closeLib(_gameLibHandle);

        _gameLibHandle = DlEncapsulation::openLib(gamePath);
        if (!_gameLibHandle) {
            std::cerr << "[ERROR] Failed to load game lib: " << DlEncapsulation::errorLib() << std::endl;
            return false;
        }

        auto createGame = reinterpret_cast<CreateGameFunc>(DlEncapsulation::getLib(_gameLibHandle, "createGame"));
        if (!createGame) {
            std::cerr << "[ERROR] createGame symbol not found: " << DlEncapsulation::errorLib() << std::endl;
            return false;
        }

        _game.reset(createGame());
        return true;
    }

    bool Core::loadGraphics(const std::string &graphicsPath) {
        _graphic.reset();
        if (_graphLibHandle)
            DlEncapsulation::closeLib(_graphLibHandle);

        _graphLibHandle = DlEncapsulation::openLib(graphicsPath);
        if (!_graphLibHandle) {
            std::cerr << "[ERROR] Failed to load graphics lib: " << DlEncapsulation::errorLib() << std::endl;
            return false;
        }

        _graphicLibLoaded = graphicsPath;
        auto createGraphics = reinterpret_cast<CreateGraphicsFunc>(DlEncapsulation::getLib(_graphLibHandle, "createGraphics"));
        if (!createGraphics) {
            std::cerr << "[ERROR] createGraphics symbol not found: " << DlEncapsulation::errorLib() << std::endl;
            return false;
        }

        _graphic.reset(createGraphics());
        return true;
    }

    void Core::changeGraphical(const std::vector<std::string> &graphicsLibs) {
        _graphic->stop();

        auto it = std::find(graphicsLibs.begin(), graphicsLibs.end(), _graphicLibLoaded);
        if (it == graphicsLibs.end()) return;

        size_t index = std::distance(graphicsLibs.begin(), it);
        size_t nextIndex = (index + 1) % graphicsLibs.size();

        loadGraphics(graphicsLibs[nextIndex]);
        _graphic->init();
    }

}
