/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** main
*/

#include "server/main.hpp"

/**
 * @brief The entry point of the program.
 * 
 * @param ac Argument count
 * @param av Argument vector
 * @return int Exit status
 */
int main(int ac, char **av)
{
    if (ac < 5 || std::string(av[1]) != "-p" || std::string(av[3]) != "-m") { // Check for correct arguments
        std::cerr << "Usage: ./jetpack_server -p <port> -m <map> [-d]" << std::endl;
        return 84;
    }

    int port = std::stoi(av[2]); // Convert port to int
    std::string mapPath = av[4]; // Map path
    bool debug = (ac == 6 && std::string(av[5]) == "-d"); // Debug mode

    try {
        Server server(port, mapPath, debug); // Create server instance
        server.run(); // Run the server
    } catch (const std::exception &e) { // Catch any exceptions
        std::cerr << "[ERROR] " << e.what() << std::endl;
        return 84;
    }

    return 0;
}
