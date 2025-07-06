/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Utils
*/

#include "client/GameRenderer.hpp"
#include "common/Packet.hpp"
#include <string>

void GameRenderer::renderGame(float dt) {
    _window.clear();

    // Render different game states
    if (_gameState == GameState::LOBBY) {
        renderLobby(); // Render the lobby screen
    } else if (_gameState == GameState::GAME) {
        renderBackground(dt); // Render the scrolling background
        renderMap(dt);        // Render the map elements
        for (const auto& [id, pos] : _nextPlayers) {
            if (id != _playerId) {
                renderPlayer(pos, 170); // Render other players with reduced opacity
            }
            renderPlayer(_myPosition, 255); // Render the current player with full opacity
        }
    } else if (_gameState == GameState::END) {
        renderEndScreen(); // Render the end screen
    }
}

void GameRenderer::renderLobby() {
    sf::Text text;
    sf::Text text2;
    sf::Text text3;

    // Display "Waiting for players..." message
    text.setString("Waiting for players...");
    sf::Font font;
    if (!font.loadFromFile("assets/jetpack_font.ttf")) {
        std::cerr << "[ERROR] Could not load font" << std::endl;
    }
    text.setFont(font);
    text.setCharacterSize(24);
    sf::FloatRect textRect = text.getLocalBounds();
    text.setOrigin(textRect.left + textRect.width / 2.0f, textRect.top + textRect.height / 2.0f);
    text.setPosition(_window.getSize().x / 2.0f, _window.getSize().y / 2.0f - 50);
    _window.draw(text);

    // Display the number of players ready
    text2.setString("Number of players ready: " + std::to_string(_nbPlayerReady) + " sur " + std::to_string(_nbPlayer));
    text2.setFont(font);
    text2.setCharacterSize(24);
    sf::FloatRect text2Rect = text2.getLocalBounds();
    text2.setOrigin(text2Rect.left + text2Rect.width / 2.0f, text2Rect.top + text2Rect.height / 2.0f);
    text2.setPosition(_window.getSize().x / 2.0f, _window.getSize().y / 2.0f);
    _window.draw(text2);

    // Display "Press R to ready up" message
    text3.setString("Press R to ready up");
    text3.setFont(font);
    text3.setCharacterSize(24);
    sf::FloatRect text3Rect = text3.getLocalBounds();
    text3.setOrigin(text3Rect.left + text3Rect.width / 2.0f, text3Rect.top + text3Rect.height / 2.0f);
    text3.setPosition(_window.getSize().x / 2.0f, _window.getSize().y / 2.0f + 50);
    _window.draw(text3);
}

void GameRenderer::renderPlayer(sf::Vector2f pos, int opacity) {
    static sf::Clock clock;
    sf::Time elapsed = clock.getElapsedTime();
    int frame = (elapsed.asMilliseconds() / 100) % 4;

    // Get the player sprite and set the animation frame
    sf::Sprite playerSprite = _assets.getSprite("player");
    playerSprite.setTextureRect(sf::IntRect(frame * (538 / 4), 0, 538 / 4, 803 / 6)); // 538x803 with 6 rows and 4 columns
    playerSprite.setPosition(pos);
    playerSprite.setScale(0.5f, 0.5f);
    playerSprite.setColor(sf::Color(255, 255, 255, opacity));

    // Draw the player sprite
    _window.draw(playerSprite);
}

void GameRenderer::renderBackground(float dt) {
    const float backSpeed = 100.0f;

    // Update the background scroll position
    _backScroll += backSpeed * dt;

    float backgroundWidth = _assets.getTexture("background").getSize().x;

    // Scale the background sprites
    _backgroundSprite.setScale(2.3f, 2.3f);
    _backgroundSprite2.setScale(2.3f, 2.3f);

    float yOffset = -100.0f; // Adjusted value to move the background up
    float scaledBackgroundWidth = backgroundWidth * 2.3f;

    // Reset the scroll position if it exceeds the background width
    if (_backScroll >= scaledBackgroundWidth)
        _backScroll = 0.0f;

    // Set the positions of the background sprites
    _backgroundSprite.setPosition(-_backScroll, yOffset);
    _backgroundSprite2.setPosition(scaledBackgroundWidth - _backScroll, yOffset);

    // Draw the background sprites
    _window.draw(_backgroundSprite);
    _window.draw(_backgroundSprite2);
}

void GameRenderer::renderEndScreen() {
    sf::Text text;
    sf::Font font;

    // Load the font for the end screen
    if (!font.loadFromFile("assets/jetpack_font.ttf"))
        std::cerr << "[ERROR] Could not load font" << std::endl;

    // Display "Game Over" message
    text.setFont(font);
    text.setCharacterSize(24);
    text.setString("Game Over");
    sf::FloatRect textRect = text.getLocalBounds();
    text.setOrigin(textRect.left + textRect.width / 2.0f, textRect.top + textRect.height / 2.0f);
    text.setPosition(_window.getSize().x / 2.0f, _window.getSize().y / 2.0f);
    _window.draw(text);
}

void GameRenderer::renderMap(float dt) {
    const float scrollSpeed = 100.0f;
    const float yOffset = 120.0f;

    // Initialize coins and zappers if not already done
    if (!_coinsInitialized) {
        _coins.clear();
        _zappers.clear();
        for (const auto& [character, position] : _mapData) {
            if (character == 'c') {
                _coins.push_back(position); // Add coin positions
            } else if (character == 'e') {
                _zappers.push_back(position); // Add zapper positions
            }
        }
        _coinsInitialized = true;
    }

    // Render coins
    for (auto& coinPos : _coins) {
        coinPos.x -= scrollSpeed * dt; // Scroll the coins
        sf::Vector2f adjustedPosition = coinPos;
        adjustedPosition.y += yOffset;
        renderCoin(adjustedPosition, coinPos);
    }

    // Render zappers
    for (auto& zapperPos : _zappers) {
        zapperPos.x -= scrollSpeed * dt; // Scroll the zappers
        sf::Vector2f adjustedPosition = zapperPos;
        adjustedPosition.y += yOffset;
        renderZapper(adjustedPosition, zapperPos);
    }
}

void GameRenderer::renderCoin(sf::Vector2f displayPos, sf::Vector2f& originalPos) {
    static sf::Clock clock;
    sf::Time elapsed = clock.getElapsedTime();
    int frame = (elapsed.asMilliseconds() / 100) % 6;

    // Get the coin sprite and set the animation frame
    sf::Sprite coinSprite = _assets.getSprite("coin");
    coinSprite.setTextureRect(sf::IntRect(frame * (1151 / 6), 0, 1151 / 6, 171));
    coinSprite.setPosition(displayPos);
    coinSprite.setScale(0.2f, 0.2f);

    // Draw the coin sprite
    _window.draw(coinSprite);

    // Check for collision between the coin and the player
    sf::FloatRect coinBounds = coinSprite.getGlobalBounds();
    sf::FloatRect playerBounds(_myPosition.x, _myPosition.y, 50.0f, 50.0f);

    if (coinBounds.intersects(playerBounds)) {
        // Notify the server about the coin collection
        _client.sendPacket({"ENTITY_HIT", {std::to_string(_playerId), "c", std::to_string(originalPos.x), std::to_string(originalPos.y)}});
        originalPos.x = -9999; // Move the coin off-screen for removal
    }
}

void GameRenderer::renderZapper(sf::Vector2f displayPos, sf::Vector2f& originalPos) {
    bool isHit = false;

    // Get the zapper sprite and set the animation frame
    sf::Sprite zapperSprite = _assets.getSprite("zapper");
    int frameWidth = zapperSprite.getTexture()->getSize().x / 5;
    zapperSprite.setTextureRect(sf::IntRect(0, 0, frameWidth, zapperSprite.getTexture()->getSize().y));
    zapperSprite.setPosition(displayPos);
    zapperSprite.setScale(0.5f, 0.5f);

    // Draw the zapper sprite
    _window.draw(zapperSprite);

    // Check for collision between the zapper and the player
    sf::FloatRect zapperBounds = zapperSprite.getGlobalBounds();
    sf::FloatRect playerBounds(_myPosition.x, _myPosition.y, 50.0f, 50.0f);

    if (zapperBounds.intersects(playerBounds) && !isHit) {
        isHit = true;
        // Notify the server about the zapper collision
        _client.sendPacket({"ENTITY_HIT", {std::to_string(_playerId), "e", std::to_string(originalPos.x), std::to_string(originalPos.y)}});
    }
}

size_t GameRenderer::getNumberOfPlayers() const {
    return _players.size(); // Return the number of players
}
