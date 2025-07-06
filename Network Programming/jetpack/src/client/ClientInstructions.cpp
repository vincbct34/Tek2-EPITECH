/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** ServerInstructions
*/

#include "client/ClientInstructions.hpp"

void IdInstructions::execute(Packet& packet, GameRenderer& renderer) {
    // Check if the packet contains arguments
    if (packet.args.empty()) return;
    try {
        // Parse the player ID from the packet and assign it to the renderer
        renderer._playerId = std::stoi(packet.args[0]);
    } catch (const std::exception& e) {
        // Log an error if parsing fails
        std::cerr << "[ERROR] Failed to parse ID: " << e.what() << std::endl;
    }
}

void PosInstructions::execute(Packet& packet, GameRenderer& renderer) {
    // Ensure the packet contains at least 3 arguments (id, x, y)
    if (packet.args.size() < 3) return;
    try {
        // Parse player ID, x, and y coordinates
        int id = std::stoi(packet.args[0]);
        float x = std::stof(packet.args[1]);
        float y = std::stof(packet.args[2]);

        // Update the player's position in the renderer
        renderer._nextPlayers[id] = {x, y};

        // If the ID matches the current player, update their position
        if (id == renderer._playerId)
            renderer._myPosition = {x, y};
    } catch (const std::exception& e) {
        // Log an error if parsing fails
        std::cerr << "[ERROR] Failed to parse POS: " << e.what() << std::endl;
    }
}

void NbPlayerReadyInstructions::execute(Packet& packet, GameRenderer& renderer) {
    // Check if the packet contains arguments
    if (packet.args.empty()) return;
    try {
        // Parse the number of players ready and update the renderer
        renderer._nbPlayerReady = std::stoi(packet.args[0]);
    } catch (const std::exception& e) {
        // Log an error if parsing fails
        std::cerr << "[ERROR] Failed to parse NbPlayerReady: " << e.what() << std::endl;
    }
}

void NbPlayerInstructions::execute(Packet& packet, GameRenderer& renderer) {
    // Check if the packet contains arguments
    if (packet.args.empty()) return;
    try {
        // Parse the total number of players and update the renderer
        renderer._nbPlayer = std::stoi(packet.args[0]);
    } catch (const std::exception& e) {
        // Log an error if parsing fails
        std::cerr << "[ERROR] Failed to parse NbPlayer: " << e.what() << std::endl;
    }
}

void MapInstructions::execute(Packet& packet, GameRenderer& renderer) {
    // Extract map data from the packet
    std::vector<std::string> mapData = packet.args;

    // Clear the existing map data in the renderer
    renderer._mapData.clear();

    // Iterate through the map data to populate the renderer
    for (std::size_t y = 0; y < mapData.size(); y++) {
        for (std::size_t x = 0; x < mapData[y].size(); x++) {
            char character = mapData[y][x];
            // Add collectible or enemy positions to the map data
            if (character == 'c' || character == 'e') {
                renderer._mapData.push_back({character, sf::Vector2f(x * TILE_SIZE, y * TILE_SIZE)});
            }
        }
    }
}

void GameStateInstructions::execute(Packet& packet, GameRenderer& renderer) {
    // Check if the packet contains arguments
    if (packet.args.empty()) return;

    // Update the game state based on the packet argument
    if (packet.args[0] == "LOBBY") {
        renderer._gameState = GameState::LOBBY;
    } else if (packet.args[0] == "GAME") {
        renderer._gameState = GameState::GAME;
    }
}

void DeathInstructions::execute(Packet& packet, GameRenderer& renderer) {
    // Check if the packet contains arguments
    if (packet.args.empty()) return;
    try {
        // If the player ID matches the current player, set the game state to END
        if (renderer._playerId == std::stoi(packet.args[0]))
            renderer._gameState = GameState::END;
    } catch (const std::exception& e) {
        // Log an error if parsing fails
        std::cerr << "[ERROR] Failed to parse DEATH: " << e.what() << std::endl;
    }
}