/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** Main.cpp
*/

#include "main.hpp"

std::atomic<bool> g_running(true);

void signalHandler(int signal) {
    if (signal == SIGINT) {
        g_running = false;
    }
}

void runSimulator(const std::string &filename) {
    Circuit circuit;
    ConfigParser parser(filename);
    ComponentFactory factory;

    parser.parse(circuit);

    for (const auto &[name, component] : circuit.getComponents()) {
        factory.setAllLinks(name, &circuit);
    }

    std::string command;
    while (true) {
        std::cout << "> ";
        std::getline(std::cin, command);

        if (command == "exit" || std::cin.eof())
            break;
        else if (command == "display")
            circuit.display();
        else if (command == "simulate")
            circuit.simulate();
        else if (command == "loop") {
            std::signal(SIGINT, signalHandler);
            while (g_running) {
                circuit.simulate();
                circuit.display();
            }
            g_running = true;
        } else if (command.find("=") != std::string::npos) {
            circuit.setValues(command);
        } else {
            std::cerr << "Error: unknown command" << std::endl;
        }
    }
}

int main(int ac, char **av) {
    int returnValue = 0;

    try {
        if (ac != 2)
            throw BadArgumentsError();
        runSimulator(av[1]);
    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
        returnValue = 84;
    }

    return returnValue;
}
