/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** main
*/

/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** client main
*/

#include "client/main.hpp"

int main(int ac, char **av)
{
    if (ac < 5 || std::string(av[1]) != "-h" || std::string(av[3]) != "-p") {
        std::cerr << "Usage: ./jetpack_client -h <ip> -p <port> [-d]" << std::endl;
        return 84;
    }

    std::string ip = av[2];
    int port = std::stoi(av[4]);
    bool debug = (ac == 6 && std::string(av[5]) == "-d");

    try {
        NetworkClient client(ip, port, debug);
        GameRenderer renderer(client);
        renderer.run();
    } catch (const std::exception& e) {
        std::cerr << "[ERROR] " << e.what() << std::endl;
        return 84;
    }

    return 0;
}
