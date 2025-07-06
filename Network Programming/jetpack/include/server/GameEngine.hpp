/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** GameEngine
*/

#pragma once

#include "common/Map.hpp"
#include "Exceptions.hpp"
#include <SFML/Graphics.hpp>

#include <unordered_map>
#include <unordered_set>

/**
 * @brief Enum representing the different states of the game
 */
enum class GameState {
    LOBBY, // Lobby state where players can connect and prepare
    GAME, // Game state where the actual game is played
    END // End state where the game is over
};

class GameEngine {
public:
    /**
     * @brief Construct a new GameEngine object
     *
     * @param map The game map instance
     */
    GameEngine(const Map& map);

    /**
     * @brief Struct representing the state of a player
     */
    struct PlayerState {
        float x, y;
        int score = 0;
        bool alive = true;
        bool ready = false;
    };

    /**
     * @brief Add a player to the game
     *
     * @param id The ID of the player
     */
    void addPlayer(int id);
    /**
     * @brief Remove a player from the game
     *
     * @param id The ID of the player
     */
    void removePlayer(int id);

    /**
     * @brief Update the position of a player
     *
     * @param id The ID of the player
     * @param x The new x position
     * @param y The new y position
     */
    void updatePlayerPosition(int id, float x, float y);
    /**
     * @brief Check if the game is over
     *
     * @return true if the game is over
     * @return false if the game is still ongoing
     */
    bool isGameOver() const;
    /**
     * @brief Check for collisions between players and the map
     */
    void checkCollisions();

    /**
     * @brief Set the game state
     *
     * @param state The new game state
     */
    void setState(GameState state);
    /**
     * @brief Set the ready state of a player
     *
     * @param id The ID of the player
     */
    void setPlayerReady(int id);

    /**
     * @brief Get the player state by ID
     *
     * @param id The ID of the player
     * @return PlayerState& The state of the player
     */
    const std::unordered_map<int, PlayerState>& getPlayerStates() const;
    /**
     * @brief Get the game state
     *
     * @return GameState The current game state
     */
    GameState getState() const;

    std::unordered_map<int, PlayerState> _players; // Map of player IDs to their states
private:
    const Map& _map; // The game map instance
    
    GameState _state; // The current game state

    bool isInsideMap(int x, int y) const; // Check if the coordinates are within the map bounds
};
