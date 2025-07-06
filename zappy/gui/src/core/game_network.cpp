/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** game_network
*/

#include "core/Game.hpp"
#include "network/NetworkClient.hpp"
#include "network/PacketParser.hpp"
#include <thread>
#include <unistd.h>

void Game::initServerConnection(char **argv) {
  if (!networkClient)
    networkClient = std::make_unique<NetworkClient>();
  try {
    networkClient->setPort(networkClient->getOption("-p", argv));
    networkClient->setHost(networkClient->getOption("-h", argv));
    networkClient->connectToServer();
    if (networkClient->getSocketFd() < 0)
      throw std::runtime_error("[ERROR] Failed to establish connection to server");
    std::cout << "[DEBUG] Successfully connected to " << networkClient->getHost() << ":" << networkClient->getPort() << std::endl;
  } catch (const std::exception& e) {
    std::cerr << "[ERROR] Connection failed: " << e.what() << std::endl;
    throw;
  }
}

void Game::initServerConnectionByGUI() {
  if (!networkClient)
    networkClient = std::make_unique<NetworkClient>();
  try {
    networkClient->setPort(getServerPort());
    networkClient->setHost(getServerIp());
    networkClient->connectToServer();
    if (networkClient->getSocketFd() < 0)
      throw std::runtime_error("Failed to establish connection");
    std::cout << "[DEBUG] Successfully connected to " << getServerIp() << ":" << getServerPort() << std::endl;
  } catch (const std::exception& e) {
    std::cerr << "[ERROR] Connection failed: " << e.what() << std::endl;
    throw;
  }
}

void Game::listenToServer() {
  if (!networkClient) {
    std::cerr << "[ERROR] NetworkClient not initialized" << std::endl;
    return;
  }
  if (!packetParser)
    packetParser = std::make_unique<PacketParser>(*this, *networkClient);

  std::thread listener([this]() {
    char buffer[1024];
    int bytesRead;
    std::string accumulatedData;

    if (networkClient->getSocketFd() < 0) {
      std::cerr << "[ERROR] Invalid socket file descriptor" << std::endl;
      setServerStatus(4);
      disconnectTimestamp = (float)CustomRayLib::getTime();
      return;
    }    
    if (networkClient->getSocketFd() < 0) {
      std::cerr << "[ERROR] Invalid socket file descriptor" << std::endl;
      return;
    }

    while (true) {
      bytesRead = read(networkClient->getSocketFd(), buffer, sizeof(buffer) - 1);
      if (bytesRead > 0) {
        buffer[bytesRead] = '\0';
        accumulatedData += buffer;

        size_t pos = 0;
        while ((pos = accumulatedData.find('\n')) != std::string::npos) {
          std::string packet = accumulatedData.substr(0, pos);
          std::cout << "Received packet: " << packet << std::endl;
          packetParser->parseAndExecute(packet);
          accumulatedData.erase(0, pos + 1);
        }
      } else if (bytesRead < 0) {
        std::cerr << "[ERROR] Error reading from server: " << std::endl;
        setServerStatus(4);
        disconnectTimestamp = (float)CustomRayLib::getTime();
        break;
      } else {
        std::cout << "[INFO] Server disconnected" << std::endl;
        setServerStatus(4);
        disconnectTimestamp = (float)CustomRayLib::getTime();
        break;
      }
    }
  });
  listener.detach();
}
