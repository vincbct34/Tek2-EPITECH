/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** Server.cpp
*/

#include "Server.hpp"
#include "SocketActions.hpp"

#define MAX_CLIENTS 100000

Server::Server(int port, const std::string& homeDir) : port(port), homeDirectory(homeDir) {}

void Server::setupSocket() {
    serverSocket = SocketActions::openSocket(AF_INET, SOCK_STREAM, 0);

    if (serverSocket == -1) {
        SocketActions::closeSocket(serverSocket);
        throw ServerError("socket: " + std::string(std::strerror(errno)));
    }

    int opt = 1;

    if (SocketActions::setSocketOption(serverSocket, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)) == -1) {
        SocketActions::closeSocket(serverSocket);
        throw ServerError("setsockopt: " + std::string(std::strerror(errno)));
    }

    sockaddr_in serverAddr;
    serverAddr.sin_family = AF_INET;
    serverAddr.sin_addr.s_addr = INADDR_ANY;
    serverAddr.sin_port = SocketActions::convertPort(port);

    if (SocketActions::bindSocket(serverSocket, (sockaddr*)&serverAddr, sizeof(serverAddr)) == -1) {
        SocketActions::closeSocket(serverSocket);
        throw ServerError("bind: " + std::string(std::strerror(errno)));
    }

    if (SocketActions::listenSocket(serverSocket, MAX_CLIENTS) == -1) {
        SocketActions::closeSocket(serverSocket);
        throw ServerError("listen: " + std::string(std::strerror(errno)));
    }

    if (SocketActions::directoryNavigation(homeDirectory.c_str()) == -1) {
        SocketActions::closeSocket(serverSocket);
        throw DirectoryHandlingError("chdir: " + std::string(std::strerror(errno)));
    }

    std::cout << "Server started on port " << port << std::endl;
}

void Server::handleClients() {
    std::vector<pollfd> fds;
    fds.push_back({serverSocket, POLLIN, 0});

    while (true) {
        int poll_count = SocketActions::pollFunction(fds.data(), fds.size(), -1);

        if (poll_count == -1) {
            SocketActions::closeSocket(serverSocket);
            throw PollError(std::strerror(errno));
        }

        for (size_t i = 0; i < fds.size(); i++) {
            if (fds[i].revents & POLLIN) {
                if (fds[i].fd == serverSocket) {
                    sockaddr_in clientAddr;
                    socklen_t clientAddrLength = sizeof(clientAddr);
                    int clientSocket = SocketActions::acceptConnection(serverSocket, (struct sockaddr*)&clientAddr, &clientAddrLength);

                    if (clientSocket <= 0) {
                        std::cerr << "Error accepting connection." << std::endl;
                        continue;
                    } else {
                        std::cout << "New client connected." << std::endl;
                        clients.try_emplace(clientSocket, std::make_unique<ClientSession>(clientSocket));
                        fds.push_back({clientSocket, POLLIN, 0});
                    }
                } else {
                    char buffer[1024];
                    int bytesRead = SocketActions::readFromSocket(fds[i].fd, buffer, sizeof(buffer));

                    if (bytesRead <= 0) {
                        std::cout << "Client disconnected." << std::endl;
                        SocketActions::closeSocket(fds[i].fd);
                        clients.erase(fds[i].fd);
                        fds.erase(fds.begin() + i);
                        i--;
                    } else {
                        buffer[bytesRead] = '\0';
                        std::cout << "Received: " << buffer << std::endl;

                        bool isQuit = clients[fds[i].fd]->handleRequest(buffer);

                        if (isQuit) {
                            SocketActions::closeSocket(fds[i].fd);
                            clients.erase(fds[i].fd);
                            fds.erase(fds.begin() + i);
                            i--;
                        }
                    }
                }
            }
        }
    }
    SocketActions::closeSocket(serverSocket);
}

void Server::run() {
    try {
        setupSocket();
        handleClients();
    } catch (const std::runtime_error& e) {
        std::cerr << e.what() << std::endl;
        std::exit(84);
    }
}
