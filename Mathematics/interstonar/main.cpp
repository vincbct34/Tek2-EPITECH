/*
** EPITECH PROJECT, 2025
** Interstonar [WSL: Ubuntu-24.04]
** File description:
** main
*/

#include "main.hpp"

bool isFloat(const std::string& str) {
    try {
        std::size_t pos;
        std::stof(str, &pos);
        return pos == str.length();
    } catch (...) {
        return false;
    }
}

int main(int argc, char **argv)
{
    if (argc != 9) {
        std::cerr << "Usage: ./interstonar [--global | --local] CONFIG_FILE Px Py Pz Vx Vy Vz" << std::endl;
        return 84;
    }

    // Check if all the position and velocity values are valid floats
    for (int i = 3; i <= 8; ++i) {
        if (!isFloat(argv[i])) {
            std::cerr << "Invalid float value: " << argv[i] << std::endl;
            return 84;
        }
    }

    std::string mode = argv[1];
    std::string config = argv[2];
    Vector3 position(std::stod(argv[3]), std::stod(argv[4]), std::stod(argv[5]));
    Vector3 velocity(std::stod(argv[6]), std::stod(argv[7]), std::stod(argv[8]));

    try {
        if (mode == "--global" || mode == "-global")
            simulateGlobal(config, position, velocity);
        else if (mode == "--local" || mode == "-local")
            simulateLocal(config, position, velocity);
        else
            throw InvalidArguments("Invalid mode. Use --global or --local.");
    } catch (const std::exception &e) {
        std::cerr << "[ERROR]: " << e.what() << std::endl;
        return 84;
    } catch (...) {
        std::cerr << "Unknown error occurred." << std::endl;
        return 84;
    }
    return 0;
}
