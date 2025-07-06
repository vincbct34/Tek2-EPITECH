/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Server
*/

#pragma once

#include "server/ClientSession.hpp"
#include "server/GameEngine.hpp"
#include "common/Packet.hpp"
#include "common/Map.hpp"

#include <optional>
#include <unordered_map>
#include <netinet/in.h>
#include <memory>

/**
 * @brief The Server class handles the server-side logic for the game.
 */
class Server {
public:
    /**
     * @brief Construct a new Server object
     * 
     * @param port The port to listen on
     * @param mapPath The path to the map file
     * @param debug Enable or disable debug mode
     */
    Server(int port, const std::string& mapPath, bool debug);
    /**
     * @brief The main function of the server-side application.
     */
    void run();

    /**
     * @brief Remove a client from the server in case of disconnection.
     * 
     * @param clientFd The file descriptor of the client to remove
     */
     void removeClient(int clientFd);
     
     int _forbiddenClientFd; // The file descriptor of the client to be removed

     private:
    int _serverSocket; // The server socket file descriptor
    int _nextClientId; // The next client ID to assign
    bool _debug; // Enable or disable debug mode
    int _port; // The port to listen on

    std::unordered_map<int, std::unique_ptr<ClientSession>> _clients; // Map of client file descriptors to client sessions

    std::optional<GameEngine> _engine; // The game engine instance

    Map _map; // The map instance

    bool hasStarted = false; // Flag to check if the game has started

    /**
     * @brief Get the number of players who are ready.
     * 
     * @return The number of ready players.
     */
    int getNbPlayerReady() const;

    /**
     * @brief Get the total number of players connected to the server.
     * 
     * @return The total number of players.
     */
    int getNbPlayer() const;

    /**
     * @brief Set up the server socket.
     * 
     * This function initializes the server socket and binds it to the specified port.
     */
    void setupSocket();

    /**
     * @brief Accept a new client connection.
     * 
     * @param fds The list of file descriptors to poll.
     * 
     * This function handles incoming client connections and adds them to the server.
     */
    void acceptNewClient(std::vector<pollfd>& fds);

    /**
     * @brief Handle data received from a client.
     * 
     * @param fds The list of file descriptors to poll.
     * @param index The index of the client in the file descriptor list.
     * 
     * This function processes incoming data from a specific client.
     */
    void handleClientData(std::vector<pollfd>& fds, int index);

    /**
     * @brief Broadcast a packet to all connected clients.
     * 
     * @param packet The packet to broadcast.
     * 
     * This function sends the specified packet to all clients currently connected to the server.
     */
    void broadcastPacket(const Packet& packet);
};
