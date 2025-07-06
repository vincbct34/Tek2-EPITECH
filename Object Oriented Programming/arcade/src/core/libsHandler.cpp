/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** libsHandler
*/

#include "Core.hpp"

namespace fs = std::filesystem;

namespace Arcade {

    const std::unordered_set<std::string> gameLibNames = {
        "arcade_menu.so", "arcade_snake.so", "arcade_centipede.so"
    };

    bool isAGameLib(const std::string &libPath) {
        std::string filename = fs::path(libPath).filename().string();
        return gameLibNames.find(filename) != gameLibNames.end();
    }

    std::vector<std::string> getGamesLibraries(const std::string &path) {
        std::vector<std::string> menuLibs, gameLibs;

        for (const auto &entry : fs::directory_iterator(path)) {
            if (entry.path().extension() == ".so" && isAGameLib(entry.path().string())) {
                if (entry.path().string().find("menu") != std::string::npos)
                    menuLibs.push_back(entry.path().string());
                else
                    gameLibs.push_back(entry.path().string());
            }
        }

        menuLibs.insert(menuLibs.end(), gameLibs.begin(), gameLibs.end());
        return menuLibs;
    }

    std::vector<std::string> getGraphicalsLibraries(const std::string &path) {
        std::vector<std::string> libs;

        for (const auto &entry : fs::directory_iterator(path)) {
            if (entry.path().extension() == ".so" && !isAGameLib(entry.path().string())) {
                libs.push_back(entry.path().string());
            }
        }

        return libs;
    }

    bool isArgValid(const std::vector<std::string> &graphicsLibs, const std::vector<std::string> &gamesLibs, char **argv) {
        if (graphicsLibs.empty()) {
            std::cerr << "[ERROR] No graphical libraries found." << std::endl;
            return false;
        }

        if (gamesLibs.empty()) {
            std::cerr << "[ERROR] No game libraries found." << std::endl;
            return false;
        }

        std::string argPath = argv[1];
        if (std::find(graphicsLibs.begin(), graphicsLibs.end(), argPath) == graphicsLibs.end() &&
            std::find(graphicsLibs.begin(), graphicsLibs.end(), std::string(argv[1] + 2)) == graphicsLibs.end()) {
            std::cerr << "[ERROR] Invalid graphical library: " << argv[1] << std::endl;
            return false;
        }

        return true;
    }

}