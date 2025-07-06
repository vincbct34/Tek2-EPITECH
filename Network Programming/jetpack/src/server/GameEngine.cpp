/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** GameEngine
*/

#include "server/GameEngine.hpp"
#include "common/Packet.hpp"
#include <cstdlib>
#include <iostream>

#define TILE_SIZE 32

GameEngine::GameEngine(const Map& map) : _map(map) {}

void GameEngine::addPlayer(int id) {
    if (_players.find(id) != _players.end())
        throw ServerError("Player ID already exists in addPlayer");

    PlayerState state{10.0, -10.0};
    _players[id] = state; // Add the player to the game
}

void GameEngine::removePlayer(int id) {
    _players.erase(id); // Remove the player from the game
}

void GameEngine::updatePlayerPosition(int id, float x, float y) {
    if (_players.find(id) == _players.end())
        throw ServerError("Unknown player ID in updatePosition");

    // Ensure the player stays within the map boundaries
    float clampedX = std::max(20.0f, std::min(x, _map.getWidth() * 40.0f - 1.0f));
    float clampedY = std::max(0.0f, std::min(y, _map.getHeight() * 35.0f + 100.0f)); // Further lower the player

    _players[id].x = clampedX; // Update the player's x position
    _players[id].y = clampedY; // Update the player's y position
}

bool GameEngine::isGameOver() const {
    int aliveCount = 0;

    for (const auto& [id, state] : _players)
        if (state.alive)
            aliveCount++; // Count the number of alive players

    return aliveCount <= 2; // Game is over if 2 or fewer players are alive
}

bool GameEngine::isInsideMap(int x, int y) const {
    return x >= 0 && y >= 0 &&
    x < static_cast<int>(_map.getWidth()) &&
    y < static_cast<int>(_map.getHeight()); // Check if the coordinates are within the map bounds
}

void GameEngine::setState(GameState state) {
    _state = state;
}

void GameEngine::setPlayerReady(int id) {
    if (_players.find(id) == _players.end()) // Check if the player ID exists
        throw ServerError("Unknown player ID in setPlayerReady");

    _players[id].ready = true; // Set the player's ready state
}

const std::unordered_map<int, GameEngine::PlayerState>& GameEngine::getPlayerStates() const {
    return _players;
}

GameState GameEngine::getState() const {
    return _state;
}
