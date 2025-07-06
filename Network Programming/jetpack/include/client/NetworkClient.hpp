/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** NetworkClient
*/

#pragma once

#include "common/Packet.hpp"
#include "SocketActions.hpp"
#include "Exceptions.hpp"

#include <arpa/inet.h>
#include <iostream>
#include <string>
#include <thread>
#include <atomic>
#include <queue>
#include <mutex>

class NetworkClient {
public:
    /**
     * @brief Constructs a NetworkClient object.
     *
     * @param ip The IP address of the server.
     * @param port The port number of the server.
     * @param debug Whether to enable debug mode.
     */
    NetworkClient(const std::string& ip, int port, bool debug);
    /**
     * @brief Destroys the NetworkClient object.
     */
    ~NetworkClient();

    /**
     * @brief Sends a packet to the server.
     *
     * @param packet The packet to send.
     */
    void sendPacket(const Packet& packet);
    /**
     * @brief Receives a packet from the server.
     *
     * @param packet The packet to receive.
     * @return true if the packet was received successfully, false otherwise.
     */
    bool receivePacket(Packet& packet);
    /**
     * @brief Checks if there are any packets in the inbox.
     *
     * @return true if there are packets, false otherwise.
     */
    bool getNextPacket(Packet& packet);

    /**
     * @brief Gets the socket file descriptor.
     *
     * @return The socket file descriptor.
     */
    int getSocketFd() const;

private:
    int _socketFd; // Socket file descriptor
    
    std::atomic<bool> _running; // Flag to control the receive loop
    
    std::queue<Packet> _inbox; // Queue to store received packets
    std::thread _recvThread; // Thread for receiving packets
    std::mutex _inboxMutex; // Mutex for thread safety
    
    bool _debug; // Debug mode flag

    /**
     * @brief Receives packets in a loop.
     */
    void receiveLoop();
};
