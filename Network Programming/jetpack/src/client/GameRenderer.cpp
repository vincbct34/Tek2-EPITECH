/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** GameRenderer
*/

#include "client/ClientFactory.hpp"
#include "client/GameRenderer.hpp"

GameRenderer::GameRenderer(NetworkClient& client) : _client(client), _window(sf::VideoMode(800, 600), "Jetpack Client") {
    _backgroundSprite2 = _assets.getSprite("background");
    _backgroundSprite = _assets.getSprite("background");

    _backgroundSprite2.setPosition(_assets.getTexture("background").getSize().x, 0);
    _backgroundSprite.setPosition(0, 0);

    _playerShape.setFillColor(sf::Color::Green);
    _playerShape.setSize({40, 40});
}

void GameRenderer::run() {
    sf::Clock clock;

    while (_window.isOpen()) {
        sf::Event event;

        while (_window.pollEvent(event)) {
            if (event.type == sf::Event::Closed) {
                _window.close();
                _client.sendPacket({"QUIT", {}});
                exit(0);
            }
        }

        float dt = clock.restart().asSeconds();
        dt = std::min(dt, 0.1f);

        handleInput(dt);
        _nextPlayers.clear();

        // Utilisation de poll pour vérifier s'il y a des données à lire sur le socket
        pollfd fds[1];
        fds[0].fd = _client.getSocketFd();
        fds[0].events = POLLIN;

        int ret = poll(fds, 1, 0); // 0ms timeout => non bloquant

        if (ret > 0 && (fds[0].revents & POLLIN)) {
            Packet packet;
            while (_client.getNextPacket(packet)) {
                ClientFactory factory;
                std::unique_ptr<ClientInstructions> instruction = factory.createInstructions(packet);

                if (instruction) {
                    instruction->execute(packet, *this);
                } else {
                    std::cerr << "[ERROR] Unknown packet type: " << packet.type << std::endl;
                }
            }
        }

        _players = _nextPlayers;
        renderGame(dt);
        _window.display();
    }
}


void GameRenderer::JetpackMovement(float dt) {
    float jetpackBoost = 600.0f;
    float gravity = 300.0f;
    sf::Vector2f velocity;

    if (_window.hasFocus() && sf::Keyboard::isKeyPressed(sf::Keyboard::Space)) {
        velocity.y -= jetpackBoost; // Apply upward force with jetpack
    }

    velocity.y += gravity; // Apply gravity when not using jetpack
    _myPosition += velocity * dt;
}

void GameRenderer::handleInput(float dt) {
    if (_gameState == GameState::GAME) {
        JetpackMovement(dt); // Update player position with jetpack movement

        _client.sendPacket({"MOVE", {
            std::to_string(_myPosition.x),
            std::to_string(_myPosition.y)
        }}); // Send player position to server
    } else if (_gameState == GameState::END) {
        if (sf::Keyboard::isKeyPressed(sf::Keyboard::Escape)) {
            _window.close();
            _client.sendPacket({"QUIT", {}});
            exit(0);
        }
    } else {
        if (_window.hasFocus() && sf::Keyboard::isKeyPressed(sf::Keyboard::R) && _gameState == GameState::LOBBY)
            _client.sendPacket({"READY", {}}); // Send ready signal to server
    }
}

void GameRenderer::render(float dt) {
    _window.clear();

    renderGame(dt); // TODO: Implement in GameEngine, not here

    _window.display();
}
