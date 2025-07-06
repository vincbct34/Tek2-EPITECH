/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** ServerFactory
*/

#include "server/ServerFactory.hpp"

std::unordered_map<std::string, std::function<std::unique_ptr<ServerInstructions>()>> ServerFactory::_factory = {
    { "MOVE", []() { return std::make_unique<MoveInstructions>(); } },
    { "QUIT\n", []() { return std::make_unique<QuitInstructions>(); } },
    { "READY\n", []() { return std::make_unique<ReadyInstructions>(); } },
    { "ENTITY_HIT", []() { return std::make_unique<EntityHitInstructions>(); } }
};

std::unique_ptr<ServerInstructions> ServerFactory::createInstructions(Packet& packet) {
    auto it = _factory.find(packet.type); // Find the instruction type in the factory map

    if (it != _factory.end()) {
        return it->second(); // Create the instruction using the factory function
    }

    return nullptr; // Return nullptr if the instruction type is not found
}
