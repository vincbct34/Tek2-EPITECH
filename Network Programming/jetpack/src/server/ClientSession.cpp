/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** ClientSession
*/

#include "server/ClientSession.hpp"
#include "SocketActions.hpp"
#include <iostream>

ClientSession::ClientSession(int socket, int id) : _socket(socket), _id(id) {}

ClientSession::~ClientSession() {
    if (_socket >= 0)
        close(_socket); // Close the socket
}

bool ClientSession::receivePacket(Packet& packet) {
    char buffer[1024];
    ssize_t len = SocketActions::readSocket(_socket, buffer, sizeof(buffer) - 1); // Read data from the socket

    if (len <= 0)
        return false; // Check for errors or disconnection

    buffer[len] = '\0';

    try {
        packet = Packet::parse(std::string(buffer)); // Parse the packet
    } catch (const std::exception& e) {
        throw ClientError("Invalid packet format: " + std::string(e.what()));
    }

    return true;
}

void ClientSession::sendPacket(const Packet& packet) {
    std::string msg = packet.serialize() + "\n";
    ssize_t sent = SocketActions::writeSocket(_socket, msg.c_str(), msg.size()); // Send data to the socket

    if (sent < 0)
        throw ClientError("Failed to send packet to client ID " + std::to_string(_id));

    if (_debug)
        std::cout << "[DEBUG] Sent packet to client ID " << _id << ": " << msg << std::endl; // Debug output
}

int ClientSession::getId() const {
    return _id;
}

int ClientSession::getSocket() const {
    return _socket;
}
