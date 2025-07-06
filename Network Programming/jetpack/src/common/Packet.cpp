/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Packet
*/

#include "common/Packet.hpp"

Packet Packet::parse(const std::string& raw) {
    Packet pkt;
    std::string arg;
    std::istringstream ss(raw);

    // Extract the packet type (before the first ':')
    std::getline(ss, pkt.type, ':');

    // Extract the arguments (separated by ':') and store them in the args vector
    while (std::getline(ss, arg, ':'))
        pkt.args.push_back(arg);

    return pkt; // Return the parsed packet
}

std::string Packet::serialize() const {
    std::ostringstream ss;

    // Write the packet type
    ss << type;

    // Append each argument, separated by ':'
    for (const auto& arg : args)
        ss << ':' << arg;

    return ss.str(); // Return the serialized packet as a string
}
