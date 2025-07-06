/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** main.cpp
*/

#include "main.hpp"
#include <filesystem>

int main(int argc, char **argv) {
    if (argc != 3) {
        std::cerr << "USAGE: " << argv[0] << " <port> <home_directory>" << std::endl;
        return 84;
    }

    int port = std::stoi(argv[1]);
    std::string homeDirectory = argv[2];

    if (std::filesystem::is_directory(homeDirectory) == false) {
        std::cerr << "Error: " << homeDirectory << " is not a directory." << std::endl;
        return 84;
    }

    Server ftpServer(port, homeDirectory);
    ftpServer.run();

    return 0;
}
