/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** ClientSession.hpp
*/

#pragma once

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <iostream>
#include <cstring>
#include <string>

#include "CommandFactory.hpp"
#include "SocketActions.hpp"

class ClientSession {
public:
    ClientSession(int socket);
    ~ClientSession();

    int clientSocket;
    int pasvSocket;
    int dataSocket;
    bool isAuthenticated;
    std::string username;

    bool handleRequest(const std::string& request);

private:
    void handleCommand(const std::string& cmd, const std::string& args);
};
