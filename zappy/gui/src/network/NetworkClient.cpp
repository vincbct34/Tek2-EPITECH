/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** NetworkClient
*/

#include "network/NetworkClient.hpp"
#include "utils/CustomSocketHandling.hpp"

NetworkClient::~NetworkClient() {
  if (socketFd != -1)
    CustomSocketHandling::closeSocket(socketFd);
}

std::string NetworkClient::getOption(const std::string &option,
                                     char **argv) const {
  for (int i = 1; i < 5; ++i) {
    if (argv[i] == option && i + 1 < 5) {
      return argv[i + 1];
    }
  }
  return "";
}

void NetworkClient::setPort(std::string port) {
  if (port.empty() || !std::all_of(port.begin(), port.end(), ::isdigit)) {
    throw ArgsException("Port must be a numeric value");
  }
  this->port = std::stoi(port);
}

void NetworkClient::setHost(std::string host) {
  if (host.empty() || std::any_of(host.begin(), host.end(), [](char c) {
        return !std::isalnum(c) && c != '.';
      })) {
    throw ArgsException("Host must be a valid hostname");
  }
  this->host = std::move(host);
}

void NetworkClient::connectToServer() {
  std::cout << "Connecting to " << host << " on port " << port << std::endl;

  socketFd = CustomSocketHandling::openSocket(AF_INET, SOCK_STREAM, 0);
  if (socketFd < 0)
    throw NetworkException("Failed to create socket");

  sockaddr_in servAddr{};
  servAddr.sin_family = AF_INET;
  servAddr.sin_port = CustomSocketHandling::customHtons(port);

  if (CustomSocketHandling::customInetPton(AF_INET, host.c_str(), &servAddr.sin_addr) <= 0)
    throw NetworkException("inet_pton() failed");
  if (CustomSocketHandling::connectToServer(socketFd, (sockaddr *)&servAddr, sizeof(servAddr)) < 0)
    throw NetworkException("connect() failed");
  std::cout << "Connected to server at " << host << ":" << port << std::endl;
}

void NetworkClient::sendPacket(const Packet &packet) {
  std::string msg = packet.serialize() + "\n";
  size_t sent = CustomSocketHandling::writeToSocket(socketFd, msg.c_str(), msg.size());

  if (sent < msg.size())
    throw NetworkException("Not all data sent");
}
