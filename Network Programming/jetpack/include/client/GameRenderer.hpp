/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** GameRenderer
*/

#pragma once

#include "client/NetworkClient.hpp"
#include "client/AssetManager.hpp"
#include "server/GameEngine.hpp"
#include "common/Packet.hpp"
#include "common/Map.hpp"

#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>
#include <SFML/System.hpp>
#include <unordered_map>
#include <iostream>
#include <vector>
#include <poll.h>

class GameRenderer {
public:
    GameRenderer(NetworkClient& client);
    /**
     * @brief The function that runs the game.
     */
    void run();

    GameState _gameState = GameState::LOBBY; // Game state
    
    std::unordered_map<int, sf::Vector2f> _nextPlayers; // The next players to be added
    std::vector<std::pair<char, sf::Vector2f>> _mapData; // The map data
    
    sf::Vector2f _myPosition; // The position of the player
    
    int _nbPlayerReady = 0; // The number of players ready
    int _playerId = -1; // The ID of the player
    int _nbPlayer = 0; // The number of players in the game

private:
    NetworkClient& _client; // The network client instance
    
    AssetManager _assets; // The asset manager instance

    Map _map; // The map instance

    sf::RectangleShape _playerShape;
    sf::Sprite _backgroundSprite2;
    sf::Sprite _backgroundSprite;
    sf::RenderWindow _window;

    std::unordered_map<int, sf::Vector2f> _players; // The players in the game
    std::vector<sf::Vector2f> _zappers; // The zappers in the game
    std::vector<sf::Vector2f> _coins; // The coins in the game

    bool _coinsInitialized = false;

    float _backScroll = 0.0f; // The background scroll speed

    /**
     * @brief The function that displays the zappers.
     * 
     * @param displayPosition The position to display the zappers.
     * @param originalPosition The original position of the zappers on the map.
     */
    void renderZapper(sf::Vector2f displayPosition, sf::Vector2f& originalPosition);
    /**
     * @brief The function that displays the coins.
     * 
     * @param displayPosition The position to display the coins.
     * @param originalPosition The original position of the coins on the map.
     */
    void renderCoin(sf::Vector2f displayPosition, sf::Vector2f& originalPosition);
    /**
     * @brief The function that displays the players.
     * 
     * @param pos The position to display the players.
     * @param opacity The opacity of the players.
     */
    void renderPlayer(sf::Vector2f pos, int opacity);
    /**
     * @brief The function that displays the background.
     * 
     * @param dt The delta time.
     */
    void renderBackground(float dt);
    /**
     * @brief The entry point of the displays.
     * 
     * @param dt The delta time.
     */
    void renderGame(float dt);
    /**
     * @brief The function that displays the map.
     *
     * @param dt The delta time.
     */
    void renderMap(float dt);
    /**
     * @brief The function that displays end screen.
     */
    void renderEndScreen();
    /**
     * @brief The function that clear, display and refresh the window.
     *
     * @param dt The delta time.
     */
    void render(float dt);
    /**
     * @brief The function that displays the lobby screen.
     */
    void renderLobby();

    /**
     * @brief Get the number of players in the game.
     * 
     * @return The number of players.
     */
    size_t getNumberOfPlayers() const;

    /**
     * @brief Handle user input during the game.
     * 
     * @param dt The delta time.
     */
    void handleInput(float dt);

    /**
     * @brief Handle the packet that contains the number of players ready.
     * 
     * @param packet The received packet.
     */
    void handleReceiveNbPlayerReady(const Packet& packet);

    /**
     * @brief Handle the packet that contains player positions.
     * 
     * @param packet The received packet.
     */
    void handleReceivePosition(const Packet& packet);

    /**
     * @brief Handle the packet that contains the number of players.
     * 
     * @param packet The received packet.
     */
    void handleReceiveNbPlayer(const Packet& packet);

    /**
     * @brief Handle the packet that contains the map data.
     * 
     * @param packet The received packet.
     */
    void handleReceiveMap(const Packet& packet);

    /**
     * @brief Handle the packet that contains the player's ID.
     * 
     * @param packet The received packet.
     */
    void handleReceiveId(const Packet& packet);

    /**
     * @brief Handle the movement of the jetpack.
     * 
     * @param dt The delta time.
     */
    void JetpackMovement(float dt);

};
