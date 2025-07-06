/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** ClientFactory
*/

#pragma once

#include "client/ClientInstructions.hpp"

#include <functional>

/**
 * @brief The factory that creates the instructions for the client.
 */
class ClientFactory {
    public:
        /**
         * @brief The function that creates the instructions for the client.
         *
         * @param packet The packet to create the instructions from.
         * @return The instructions for the client.
         */
        static std::unique_ptr<ClientInstructions> createInstructions(Packet& packet);
    private:
        static std::unordered_map<std::string, std::function<std::unique_ptr<ClientInstructions>()>> _factory; // Factory map
};
