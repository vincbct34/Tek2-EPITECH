/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** ClientFactory
*/

#include "client/ClientFactory.hpp"

std::unordered_map<std::string, std::function<std::unique_ptr<ClientInstructions>()>> ClientFactory::_factory = {
    {"ID", []() { return std::make_unique<IdInstructions>(); }},
    {"POS", []() { return std::make_unique<PosInstructions>(); }},
    {"NB_PLAYER_READY", []() { return std::make_unique<NbPlayerReadyInstructions>(); }},
    {"NB_PLAYER", []() { return std::make_unique<NbPlayerInstructions>(); }},
    {"MAP", []() { return std::make_unique<MapInstructions>(); }},
    {"GAME_STATE", []() { return std::make_unique<GameStateInstructions>(); }},
    {"DEATH", []() { return std::make_unique<DeathInstructions>(); }}
};

std::unique_ptr<ClientInstructions> ClientFactory::createInstructions(Packet& packet) {
    auto it = _factory.find(packet.type); // Find the instruction type in the factory map

    if (it != _factory.end()) {
        return it->second(); // Create the instruction using the factory function
    }

    return nullptr; // Return nullptr if the instruction type is not found
}
