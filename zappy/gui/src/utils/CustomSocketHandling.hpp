/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** CustomSocketHandling
*/

#pragma once

#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

class CustomSocketHandling {
public:
  /**
   * @brief Opens a socket with the specified parameters.
   *
   * @param domain The communication domain (e.g., AF_INET for IPv4).
   * @param type The type of socket (e.g., SOCK_STREAM for TCP).
   * @param protocol The protocol to be used (0 for default).
   * @return The file descriptor of the opened socket, or -1 on failure.
   */
  static int openSocket(int domain, int type, int protocol) {
    return socket(domain, type, protocol);
  }

  /**
   * @brief Closes a socket.
   *
   * @param socketFd The file descriptor of the socket to close.
   */
  static void closeSocket(int socketFd) { close(socketFd); }

  /**
   * @brief Converts a port number from host byte order to network byte order.
   *
   * @param port The port number in host byte order.
   * @return The port number in network byte order.
   */
  static uint16_t customHtons(uint16_t port) { return htons(port); }

  /**
   * @brief Converts an IP address from text to binary form.
   *
   * @param af The address family (e.g., AF_INET for IPv4).
   * @param src The source IP address in text form.
   * @param dst Pointer to the destination structure where the binary address
   * will be stored.
   * @return 1 on success, 0 if the address is not valid, or -1 on error.
   */
  static int customInetPton(int af, const char *src, void *dst) {
    return inet_pton(af, src, dst);
  }

  /**
   * @brief Connect to a server using the specified socket file descriptor and
   * address.
   *
   * @param socketFd The file descriptor of the socket to connect.
   * @param addr Pointer to the sockaddr structure containing the server
   * address.
   * @param addrlen The length of the address structure.
   * @return 0 on success, or -1 on error.
   */
  static int connectToServer(int socketFd, const sockaddr *addr,
                             socklen_t addrlen) {
    return connect(socketFd, addr, addrlen);
  }

  /**
   * @brief Writes data to a socket.
   *
   * @param socketFd The file descriptor of the socket to write to.
   * @param buffer Pointer to the data to write.
   * @param length The number of bytes to write.
   * @return The number of bytes written, or -1 on error.
   */
  static ssize_t writeToSocket(int socketFd, const void *buffer,
                               size_t length) {
    return write(socketFd, buffer, length);
  }
};
