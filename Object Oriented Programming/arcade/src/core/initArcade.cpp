/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** initArcade
*/

#include "Core.hpp"

namespace Arcade {

    void startArcade(char **argv) {
        const std::string libPath = "lib/";

        std::vector<std::string> graphicsLibs = getGraphicalsLibraries(libPath);
        std::vector<std::string> gamesLibs = getGamesLibraries(libPath);

        if (!isArgValid(graphicsLibs, gamesLibs, argv)) {
            std::cerr << "[ERROR] Invalid startup argument." << std::endl;
            return;
        }

        Core core;

        if (!core.loadGraphics(argv[1])) {
            std::cerr << "[ERROR] Could not load graphics library." << std::endl;
            return;
        }

        if (!core.loadGame(gamesLibs[0])) {
            std::cerr << "[ERROR] Could not load default game (menu)." << std::endl;
            return;
        }

        core.run(gamesLibs, graphicsLibs);
    }

}
