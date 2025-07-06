/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** ServerInstructions
*/

#include "server/ServerInstructions.hpp"
#include <iostream>

void MoveInstructions::execute(Packet& packet, int clientFd, GameEngine& _engine,
                               std::unordered_map<int, std::unique_ptr<ClientSession>>& _clients, Server& server) {
    (void)server; // Unused parameter

    // Ensure the packet contains at least two arguments (x and y coordinates)
    if (packet.args.size() >= 2) {
        float x = std::stof(packet.args[0]); // Parse x-coordinate
        float y = std::stof(packet.args[1]); // Parse y-coordinate

        // Update the player's position in the game engine
        _engine.updatePlayerPosition(_clients[clientFd]->getId(), x, y);
    }
}

void QuitInstructions::execute(Packet& packet, int clientFd, GameEngine& _engine,
                               std::unordered_map<int, std::unique_ptr<ClientSession>>& _clients, Server& server) {
    (void)packet; // Unused parameter
    (void)_engine; // Unused parameter
    (void)_clients; // Unused parameter

    // Remove the client from the server
    server.removeClient(clientFd);
}

void ReadyInstructions::execute(Packet& packet, int clientFd, GameEngine& _engine,
                                std::unordered_map<int, std::unique_ptr<ClientSession>>& _clients, Server& server) {
    (void)packet; // Unused parameter
    (void)_engine; // Unused parameter
    (void)_clients; // Unused parameter
    (void)server; // Unused parameter

    // Mark the player as ready in the game engine
    _engine.setPlayerReady(_clients[clientFd]->getId());
}

void EntityHitInstructions::execute(Packet& packet, int clientFd, GameEngine& _engine,
                                     std::unordered_map<int, std::unique_ptr<ClientSession>>& _clients, Server& server) {
    (void)_clients; // Unused parameter
    (void)server; // Unused parameter
    (void)clientFd; // Unused parameter

    // Ensure the packet contains at least four arguments
    if (packet.args.size() >= 4) {
        try {
            int id = std::stoi(packet.args[0]); // Parse the player ID
            char type = packet.args[1][0];      // Parse the entity type ('c' for coin, 'e' for zapper)

            if (type == 'c') {
                // Increment the player's score if they hit a coin
                _engine._players[id].score++;
            } else if (type == 'e') {
                // Mark the player as not alive if they hit a zapper
                _engine._players[id].alive = false;
                exit(0); // Exit the program (this may need to be handled differently)
            }
        } catch (const std::exception& e) {
            // Log an error if parsing fails
            std::cerr << "[ERROR] Failed to parse ENTITY_HIT: " << e.what() << std::endl;
        }
    }
}
