/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** ServerInstructions
*/

#pragma once

#include "server/ClientSession.hpp"
#include "server/GameEngine.hpp"
#include "server/Server.hpp"
#include "common/Packet.hpp"

#include <memory>

/**
 * @brief The base class for the server instructions.
 */
class ServerInstructions {
public:
    /**
     * @brief The default constructor for the server instructions.
     */
    virtual ~ServerInstructions() = default;

    /**
     * @brief The function that executes the instructions.
     *
     * @param packet The packet to execute the instructions from.
     * @param clientFd The file descriptor of the client.
     * @param engine The game engine.
     * @param clients The list of clients.
     * @param server The server.
     */
    virtual void execute(Packet& packet, int clientFd, GameEngine& engine, std::unordered_map<int, std::unique_ptr<ClientSession>>& clients, Server& server) = 0;
};

/**
 * @brief The class that handles the [MOVE...] instruction.
 */
class MoveInstructions : public ServerInstructions {
public:
    void execute(Packet& packet, int clientFd, GameEngine& engine, std::unordered_map<int, std::unique_ptr<ClientSession>>& clients, Server& server) override;
};

/**
 * @brief The class that handles the [QUIT\n] instruction.
 */
class QuitInstructions : public ServerInstructions {
public:
    void execute(Packet& packet, int clientFd, GameEngine& engine, std::unordered_map<int, std::unique_ptr<ClientSession>>& clients, Server& server) override;
};

/**
 * @brief The class that handles the [READY\n] instruction.
 */
class ReadyInstructions : public ServerInstructions {
public:
    void execute(Packet& packet, int clientFd, GameEngine& engine, std::unordered_map<int, std::unique_ptr<ClientSession>>& clients, Server& server) override;
};

/**
 * @brief The class that handles the [ENTITY_POS...] instruction.
 */
class EntityHitInstructions : public ServerInstructions {
public:
    void execute(Packet& packet, int clientFd, GameEngine& engine, std::unordered_map<int, std::unique_ptr<ClientSession>>& clients, Server& server) override;
};