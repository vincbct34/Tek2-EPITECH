/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1
*/

#include <cstdlib>
#include <iostream>
#include <fstream>

void printFile(const std::string &filename) {
    std::ifstream file(filename);
    std::string line;

    if (!file.is_open()) {
        std::cerr << "MyCat: " << filename << ": No such file or directory" << std::endl;
        exit(84);
    }

    while (std::getline(file, line))
        std::cout << line << std::endl;

    file.close();
}

int main(int argc, char **argv) {
    std::string line;

    if (argc == 1)
        while (std::getline(std::cin, line))
            std::cout << line << std::endl;
    else
        for (int i = 1; i < argc; ++i)
            printFile(argv[i]);

    return 0;
}
