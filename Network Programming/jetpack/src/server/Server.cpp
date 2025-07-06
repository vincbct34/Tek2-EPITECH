/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Server
*/

#include "server/GameEngine.hpp"
#include "server/ServerFactory.hpp"
#include "server/Server.hpp"

Server::Server(int port, const std::string& mapPath, bool debug)
    : _serverSocket(-1), _nextClientId(0), _debug(debug), _port(port),
    _map() {

    // Load the map from the specified file
    if (!_map.loadFromFile(mapPath))
        throw MapError("Failed to load map from file: " + mapPath);

    // Initialize the game engine with the loaded map
    _engine.emplace(_map);
    setupSocket(); // Set up the server socket

    (void)_debug; // Debug flag (currently unused)
    _engine->setState(GameState::LOBBY); // Set the initial game state to LOBBY
}

void Server::setupSocket() {
    // Create a socket
    _serverSocket = SocketActions::openSocket(AF_INET, SOCK_STREAM, 0);

    if (_serverSocket < 0) // Check if socket creation failed
        throw SocketError("socket() failed");

    int opt = 1;

    // Set socket options to allow address reuse
    if (SocketActions::setSocketOptions(_serverSocket, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)) < 0)
        throw SocketError("setsockopt() failed");

    sockaddr_in addr{};
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = SocketActions::convertPort(_port);

    // Bind the socket to the specified port
    if (SocketActions::bindSocket(_serverSocket, (struct sockaddr *)&addr, sizeof(addr)) < 0)
        throw SocketError("bind() failed");

    // Start listening for incoming connections
    if (SocketActions::listenSocket(_serverSocket, 5) < 0)
        throw SocketError("listen() failed");

    std::cout << "[INFO] Server listening on port " << _port << std::endl;
}

void Server::run() {
    std::vector<pollfd> fds;
    fds.push_back({ .fd = _serverSocket, .events = POLLIN, .revents = 0 });

    // Main server loop
    while (true) {
        // Wait for events on the file descriptors
        if (SocketActions::waitFd(fds.data(), fds.size(), -1) < 0)
            throw SocketError("poll() failed");

        for (size_t i = 0; i < fds.size(); ++i) {
            if (fds[i].revents & POLLIN) {
                if (fds[i].fd == _serverSocket) {
                    acceptNewClient(fds); // Accept a new client connection
                } else {
                    handleClientData(fds, i); // Handle data from an existing client
                }
            }
        }
    }
    SocketActions::closeSocket(_serverSocket); // Close the server socket when done
}

void Server::acceptNewClient(std::vector<pollfd>& fds) {
    sockaddr_in clientAddr{};
    socklen_t len = sizeof(clientAddr);
    int clientFd = SocketActions::acceptConnection(_serverSocket, (sockaddr*)&clientAddr, &len);

    if (clientFd < 0)
        return;

    int clientId = _nextClientId++;

    // Create a new client session
    _clients[clientFd] = std::make_unique<ClientSession>(clientFd, clientId);
    _engine->addPlayer(clientId); // Add the player to the game engine

    // Send the client their ID
    Packet id_packet{ "ID", { std::to_string(clientId) } };
    _clients[clientFd]->sendPacket(id_packet);

    // Send the map data to the client
    Packet map_packet{ "MAP", { _map.getData() } };
    _clients[clientFd]->sendPacket(map_packet);

    // Add the client to the list of file descriptors to monitor
    fds.push_back({ .fd = clientFd, .events = POLLIN, .revents = 0 });
}

void Server::handleClientData(std::vector<pollfd>& fds, int index) {
    int clientFd = fds[index].fd;

    Packet pkt;

    // Receive a packet from the client
    if (!_clients[clientFd]->receivePacket(pkt)) {
        if (fds[index].revents & (POLLHUP | POLLERR)) {
            removeClient(clientFd); // Remove the client if disconnected
        }
        return;
    }

    ServerFactory factory;

    // Create the appropriate instruction based on the packet type
    std::unique_ptr<ServerInstructions> instruction = factory.createInstructions(pkt);

    if (instruction) {
        instruction->execute(pkt, clientFd, *_engine, _clients, *this); // Execute the instruction
    } else {
        std::cerr << "[ERROR] Unknown instruction: " << pkt.type << std::endl;
        return;
    }

    // Broadcast player states to all clients
    for (const auto& [id, state] : _engine->getPlayerStates()) {
        if (!state.alive) {
            // Send a death packet to the client if the player is dead
            Packet deadPacket{ "DEATH", { std::to_string(id) } };
            _clients[id]->sendPacket(deadPacket);
            continue; // Skip broadcasting position for dead players
        } else {
            // Broadcast the player's position
            Packet update{ "POS", { std::to_string(id), std::to_string(state.x), std::to_string(state.y) } };
            broadcastPacket(update);
        }
    }

    // Handle game state transitions
    if (_engine->getState() != GameState::END) {
        if (_engine->getState() == GameState::LOBBY) {
            Packet nbPlayerReady{ "NB_PLAYER_READY", { std::to_string(getNbPlayerReady()) } };
            Packet nbPlayer{ "NB_PLAYER", { std::to_string(getNbPlayer()) } };
            broadcastPacket(nbPlayerReady);
            broadcastPacket(nbPlayer);
        }

        // Start the game if all players are ready and there are enough players
        if (getNbPlayerReady() == getNbPlayer() && getNbPlayer() >= 2 && !hasStarted) {
            _engine->setState(GameState::GAME);
            Packet startGame{ "GAME_STATE", { "GAME" } };
            broadcastPacket(startGame);
            hasStarted = true;
        }
    }
}

int Server::getNbPlayerReady() const {
    int nbPlayerReady = 0;

    // Count the number of players who are ready
    for (const auto& [id, state] : _engine->getPlayerStates()) {
        if (state.ready)
            nbPlayerReady++;
    }
    return nbPlayerReady;
}

int Server::getNbPlayer() const {
    // Return the total number of players
    return _engine->getPlayerStates().size();
}

void Server::removeClient(int clientFd) {
    int id = _clients[clientFd]->getId();

    // Remove the client from the server and the game engine
    _clients.erase(clientFd);
    _engine->removePlayer(id);

    close(clientFd); // Close the client socket
}

void Server::broadcastPacket(const Packet& packet) {
    for (auto it = _clients.begin(); it != _clients.end(); ) {
        try {
            // Send the packet to each client
            it->second->sendPacket(packet);
            ++it;
        } catch (const std::exception& e) {
            std::cerr << "[ERROR] " << e.what() << std::endl;

            int id = it->second->getId();

            // Remove the client if sending the packet fails
            it = _clients.erase(it);
            _engine->removePlayer(id);

            close(it->first); // Close the client socket
        }
    }
}
