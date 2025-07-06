/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** Server.hpp
*/

#pragma once

#include <sys/socket.h>
#include <arpa/inet.h>
#include <algorithm>
#include <unistd.h>
#include <iostream>
#include <cstring>
#include <poll.h>

#include "CommandFactory.hpp"
#include "ClientSession.hpp"
#include "SocketActions.hpp"
#include "Exceptions.hpp"

class Server {
public:
    Server(int port, const std::string& homeDir);

    void run();
private:
    int port;
    std::string homeDirectory;
    int serverSocket;
    std::unordered_map<int, std::unique_ptr<ClientSession>> clients;

    void setupSocket();
    void handleClients();
};
