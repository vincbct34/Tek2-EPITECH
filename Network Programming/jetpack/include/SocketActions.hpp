/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** SocketActions
*/

#pragma once

#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>
#include <poll.h>

/**
 * @brief The class used to encapsulate the C functions related to socket operations.
 */
class SocketActions {
public:
    /**
     * @brief Creates a socket.
     * 
     * @param domain 
     * @param type 
     * @param protocol 
     * @return int 
     */
    static int openSocket(int domain, int type, int protocol) {
        return socket(domain, type, protocol);
    }

    /**
     * @brief Sets socket options.
     * 
     * @param socket 
     * @param level 
     * @param optname 
     * @param optval 
     * @param optlen 
     * @return int 
     */
    static int setSocketOptions(int socket, int level, int optname, const void *optval, socklen_t optlen) {
        return setsockopt(socket, level, optname, optval, optlen);
    }

    /**
     * @brief Converts a port number to network byte order.
     * 
     * @param port 
     * @return int 
     */
    static int convertPort(int port) {
        return htons(port);
    }

    /**
     * @brief Binds a socket to an address.
     * 
     * @param socket 
     * @param address 
     * @param address_len 
     * @return int 
     */
    static int bindSocket(int socket, const struct sockaddr *address, socklen_t address_len) {
        return bind(socket, address, address_len);
    }

    /**
     * @brief Listens for incoming connections on a socket.
     * 
     * @param socket 
     * @param backlog 
     * @return int 
     */
    static int listenSocket(int socket, int backlog) {
        return listen(socket, backlog);
    }

    /**
     * @brief Waits for events on a file descriptor.
     * 
     * @param fds 
     * @param nfds 
     * @param timeout 
     * @return int 
     */
    static int waitFd(struct pollfd* fds, nfds_t nfds, int timeout) {
        return poll(fds, nfds, timeout);
    }

    /**
     * @brief Accepts a new connection on a socket.
     * 
     * @param socket 
     * @param address 
     * @param address_len 
     * @return int 
     */
    static int acceptConnection(int socket, struct sockaddr *address, socklen_t *address_len) {
        return accept(socket, address, address_len);
    }

    /**
     * @brief Closes a socket.
     * 
     * @param socket 
     */
    static void closeSocket(int socket) {
        close(socket);
    }

    /**
     * @brief Sends data to a socket.
     * 
     * @param socket 
     * @param buffer 
     * @param length 
     * @return int 
     */
    static int writeSocket(int socket, const char *buffer, size_t length) {
        return write(socket, buffer, length);
    }

    /**
     * @brief Reads data from a socket.
     * 
     * @param socket 
     * @param buffer 
     * @param length 
     * @return int 
     */
    static int readSocket(int socket, char *buffer, size_t length) {
        return read(socket, buffer, length);
    }
};
