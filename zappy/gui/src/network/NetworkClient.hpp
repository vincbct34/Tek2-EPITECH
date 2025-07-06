/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** NetworkClient
*/

#pragma once

#include "network/Packet.hpp"
#include "utils/CustomSocketHandling.hpp"
#include "utils/Exceptions.hpp"

#include <algorithm>
#include <iostream>

class NetworkClient {
public:
  NetworkClient() = default;
  ~NetworkClient();

  // Args handling
  /** @brief Gets the value of a command-line option.
   *
   *  @param option The option to look for (e.g., "-p" or "-h").
   *  @param argv The command-line arguments.
   *  @return The value of the option as a string, or an empty string if not
   * found.
   */
  std::string getOption(const std::string &option, char **argv) const;
  /** @brief Sets the host address.
   *
   *  @param host The host address as a string.
   *  @throws ArgsException if the host is not a valid hostname.
   */
  void setHost(std::string host);
  /** @brief Sets the port number.
   *
   *  @param port The port number as a string.
   *  @throws ArgsException if the port is not a valid numeric value.
   */
  void setPort(std::string port);

  /** @brief Connects to the server using the specified host and port.
   *
   *  @throws NetworkException if the connection fails.
   */
  void connectToServer();

  /** @brief Sends a packet to the server.
   *
   *  @param packet The packet to send.
   *  @throws NetworkException if sending the packet fails.
   */
  void sendPacket(const Packet &packet);

  // Getters
  /** @brief Gets the host address.
   *
   *  @return The host address as a string.
   */
  std::string getHost() const { return host; }
  /** @brief Gets the port number.
   *
   *  @return The port number as an integer.
   */
  int getPort() const { return port; }
  /** @brief Gets the socket file descriptor.
   *
   *  @return The socket file descriptor as an integer.
   */
  int getSocketFd() const { return socketFd; }

private:
  std::string host;  // Host address of the server
  int port;          // Port number of the server
  int socketFd = -1; // File descriptor for the socket, initialized to -1
};
