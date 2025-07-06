/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** SocketActions.hpp
*/

#pragma once

#include <sys/socket.h>
#include <netinet/in.h>
#include <filesystem>
#include <unistd.h>
#include <poll.h>

class SocketActions {
    public:
        static int openSocket(int domain, int type, int protocol) {
            return socket(domain, type, protocol);
        }
        static int bindSocket(int socket, const struct sockaddr* address, socklen_t addressLength) {
            return bind(socket, address, addressLength);
        }
        static int listenSocket(int socket, int backlog) {
            return listen(socket, backlog);
        }
        static int setSocketOption(int socket, int level, int optionName, const void* optionValue, socklen_t optionLength) {
            return setsockopt(socket, level, optionName, optionValue, optionLength);
        }
        static int getSocketName(int socket, struct sockaddr* address, socklen_t* addressLength) {
            return getsockname(socket, address, addressLength);
        }
        static int convertPort(int port) {
            return htons(port);
        }
        static int readFromSocket(int socket, char* buffer, size_t size) {
            return read(socket, buffer, size);
        }
        static int writeToSocket(int socket, const char* buffer, size_t size) {
            return write(socket, buffer, size);
        }
        static int directoryNavigation(const char* path) {
            return chdir(path);
        }
        static int acceptConnection(int socket, struct sockaddr* address, socklen_t* addressLength) {
            return accept(socket, address, addressLength);
        }
        static void closeSocket(int socket) {
            close(socket);
        }
        static int pollFunction(struct pollfd* fds, nfds_t nfds, int timeout) {
            return poll(fds, nfds, timeout);
        }
        static int removeFile(const char* path) {
            return remove(path);
        }
        static int getPort(int port) {
            return ntohs(port);
        }
        static void exitProcess(int status) {
            _exit(status);
        }
        static int forkProcess() {
            return fork();
        }
};
