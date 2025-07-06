/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** ClientInstructions
*/

#pragma once

#include "client/GameRenderer.hpp"

#define TILE_SIZE 32

/**
 * @brief The base class for client instructions.
 */
class ClientInstructions {
public:
    /**
     * @brief The default constructor for client instructions.
     */
    virtual ~ClientInstructions() = default;

    /**
     * @brief The function that executes the instructions.
     *
     * @param packet The packet containing the instruction data.
     * @param renderer The game renderer to execute the instruction with.
     */
    virtual void execute(Packet& packet, GameRenderer& renderer) = 0;
};

/**
 * @brief The class for [ID...] instruction.
 */
class IdInstructions : public ClientInstructions {
public:
    void execute(Packet& packet, GameRenderer& renderer) override;
};

/**
 * @brief The class for [POS...] instruction.
 */
class PosInstructions : public ClientInstructions {
public:
    void execute(Packet& packet, GameRenderer& renderer) override;
};

/**
 * @brief The class for [NB_PLAYER_READY...] instruction.
 */
class NbPlayerReadyInstructions : public ClientInstructions {
public:
    void execute(Packet& packet, GameRenderer& renderer) override;
};

/**
 * @brief The class for [NB_PLAYER...] instruction.
 */
class NbPlayerInstructions : public ClientInstructions {
public:
    void execute(Packet& packet, GameRenderer& _renderer) override;
};

/**
 * @brief The class for [MAP...] instruction.
 */
class MapInstructions : public ClientInstructions {
public:
    void execute(Packet& packet, GameRenderer& renderer) override;
};

/**
 * @brief The class for [GAME_STATE...] instruction.
 */
class GameStateInstructions : public ClientInstructions {
public:
    void execute(Packet& packet, GameRenderer& renderer) override;
};

/**
 * @brief The class for [REMOVE_COIN...] instruction.
 */
class RemoveCoinInstructions : public ClientInstructions {
public:
    void execute(Packet& packet, GameRenderer& renderer) override;
};

/**
 * @brief The class for [DEATH...] instruction.
 */
class DeathInstructions : public ClientInstructions {
public:
    void execute(Packet& packet, GameRenderer& renderer) override;
};