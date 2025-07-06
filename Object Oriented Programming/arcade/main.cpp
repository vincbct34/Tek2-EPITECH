/*
** EPITECH PROJECT, 2025
** B-OOP-400-MPL-4-1-arcade-vincent.bichat
** File description:
** main
*/

#include "Core.hpp"

int main(int argc, char **argv) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << ".arcade ./src/lib/graphicals/<graphique>.so" << std::endl;
        return 84;
    }

    try {
        Arcade::startArcade(argv);
    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
        return 84;
    }
    return 0;
}
