/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** ClientSession.cpp
*/

#include "ClientSession.hpp"
#include "Exceptions.hpp"
#include <iostream>

ClientSession::ClientSession(int socket) : clientSocket(socket), pasvSocket(-1), isAuthenticated(false) {
    std::string welcomeMessage = "220 Service ready.\r\n";

    if (SocketActions::writeToSocket(clientSocket, welcomeMessage.c_str(), welcomeMessage.size()) < 0)
        throw WriteError("Failed to send welcome message.");
}

ClientSession::~ClientSession() {
    SocketActions::closeSocket(clientSocket);
    std::cout << "Client session closed." << std::endl;
}

void ClientSession::handleCommand(const std::string& cmd, const std::string& args) {
    auto command = CommandFactory::createCommand(cmd);

    if (command) {
        std::string response = command->execute(args, *this);
        if (response != "") {
            if (SocketActions::writeToSocket(clientSocket, response.c_str(), response.size()) < 0)
                throw WriteError("Failed to send response.");
        }
    } else {
        std::string response = "500 Unknown command.\r\n";

        if (SocketActions::writeToSocket(clientSocket, response.c_str(), response.size()) < 0)
            throw WriteError("Failed to send response.");
    }
}

bool ClientSession::handleRequest(const std::string& request) {
    std::string command = request.substr(0, request.find(' '));
    std::string args = request.substr(request.find(' ') + 1);

    if (command == "QUIT\r\n") {
        SocketActions::closeSocket(clientSocket);
        return true;
    }

    if (!isAuthenticated && command != "USER" && command.find("PASS") == std::string::npos) {
        if (SocketActions::writeToSocket(clientSocket, "530 Not logged in.\r\n", 20) < 0)
            throw WriteError("Failed to send response.");
        return false;
    }

    handleCommand(command, args);

    return false;
}
