/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** main
*/

#include "core/Game.hpp"
#include "network/NetworkClient.hpp"
#include "network/PacketParser.hpp"
#include "utils/Exceptions.hpp"
#include <thread>
#include <unistd.h>

void displayUsage(char **argv)
{
  std::cerr << "Usage: " << argv[0] << " -p <port> -h <hostname>" << std::endl;
  std::cerr << "Note: You can also launch the program without arguments and set IP/port in-game" << std::endl;
  std::cerr << "Options:" << std::endl;
  std::cerr << "  -p <port>      Port number to connect to the server" << std::endl;
  std::cerr << "  -h <hostname>  Hostname or IP address of the server" << std::endl;
}

int main(int argc, char **argv) {
  if (argc < 5) {
    displayUsage(argv);
    try {
      Game game;
      game.run();
    } catch (const std::exception &e) {
      std::cerr << e.what() << std::endl;
      return 84;
    }
  } else {
    try {
      Game game(1);
      try {
        game.initServerConnection(argv);
        game.listenToServer();
        game.setGuiIsListening(true);
      } catch (const std::exception &e) {
        std::cerr << "[ERROR] Failed to connect: " << e.what() << std::endl;
        game.setServerStatus(4);
        game.disconnectTimestamp = (float)CustomRayLib::getTime();
      }
      game.run();
    } catch (const std::exception &e) {
      std::cerr << e.what() << std::endl;
      return 84;
    }
  }
  return 0;
}
