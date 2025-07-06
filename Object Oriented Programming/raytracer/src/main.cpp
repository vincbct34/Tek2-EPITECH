/*
** EPITECH PROJECT, 2025
** B-OOP-400-MPL-4-1-raytracer-vincent.bichat
** File description:
** main
*/

#include "Exceptions.hpp"

#include <iostream>

void core_run(char *filename);

void display_usage()
{
    std::cout << "USAGE: ./raytracer <SCENE_FILE>" << std::endl;
    std::cout << "\tSCENE_FILE: scene configuration." << std::endl;
}

int main(int argc, char **argv)
{
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <input_file>" << std::endl;
        return 1;
    }

    if (std::string(argv[1]) == "--help") {
        display_usage();
        return 0;
    }

    try {
        core_run(argv[1]);
    } catch (const Exception &e) {
        std::cerr << e.what() << std::endl;
        return 84;
    }
    return 0;
}