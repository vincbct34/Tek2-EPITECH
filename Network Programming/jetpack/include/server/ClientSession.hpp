/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** ClientSession
*/

#pragma once

#include "common/Packet.hpp"
#include "SocketActions.hpp"
#include "Exceptions.hpp"

#include <iostream>
#include <unistd.h>
#include <cstring>

class ClientSession {
public:
    /**
     * @brief Construct a new Client Session object
     * 
     * @param socket The socket file descriptor for the client
     * @param id The ID of the client session
     */
    ClientSession(int socket, int id);
    /**
     * @brief Destroy the Client Session object
     * 
     * Closes the socket and cleans up resources.
     */
    ~ClientSession();
    
    /**
     * @brief Send a packet to the client.
     * 
     * @param packet The packet to send
     */
    void sendPacket(const Packet& packet);
    /**
     * @brief Receive a packet from the client.
     * 
     * @param packet The packet to receive
     * @return true if the packet was received successfully, false otherwise
     */
    bool receivePacket(Packet& packet);

    /**
     * @brief Get the socket file descriptor for the client.
     * 
     * @return The socket file descriptor
     */
    int getSocket() const;
    /**
     * @brief Get the ID of the client session.
     * 
     * @return The ID of the client session
     */
    int getId() const;

    bool _debug = false; // Debug mode flag
private:
    int _socket; // The socket file descriptor for the client
    int _id; // The ID of the client session

};
