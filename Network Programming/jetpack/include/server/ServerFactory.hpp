/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** ServerFactory
*/

#pragma once

#include "server/ServerInstructions.hpp"

#include <functional>

/**
 * @brief The factory that creates the instructions for the server.
 */
class ServerFactory {
    public:
        /**
         * @brief Creates the instructions for the server.
         *
         * @param packet The packet to create the instructions from.
         * @return The instructions for the server.
         */
        static std::unique_ptr<ServerInstructions> createInstructions(Packet& packet);
    private:
        static std::unordered_map<std::string, std::function<std::unique_ptr<ServerInstructions>()>> _factory; // The factory map
};
