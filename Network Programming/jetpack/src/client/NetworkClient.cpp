/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** NetworkClient
*/

#include "client/NetworkClient.hpp"
#include <cstdlib>

NetworkClient::NetworkClient(const std::string& ip, int port, bool debug)
    : _running(true), _debug(debug)
{
    _socketFd = socket(AF_INET, SOCK_STREAM, 0); // Create a socket
    if (_socketFd < 0) // Check if socket creation failed
        throw NetworkError("socket() failed");

    sockaddr_in servAddr{}; // Initialize the server address structure
    servAddr.sin_family = AF_INET; // IPv4
    servAddr.sin_port = htons(port); // Convert port to network byte order
    if (inet_pton(AF_INET, ip.c_str(), &servAddr.sin_addr) <= 0) // Convert IP address from text to binary form
        throw NetworkError("inet_pton() failed");

    if (connect(_socketFd, (sockaddr*)&servAddr, sizeof(servAddr)) < 0) // Connect to the server
        throw NetworkError("connect() failed");
    
    std::cout << "[INFO] Connected to server at " << ip << ":" << port << std::endl;

    _recvThread = std::thread(&NetworkClient::receiveLoop, this); // Start the receive loop in a separate thread
}

NetworkClient::~NetworkClient() {
    _running = false;

    if (_recvThread.joinable()) // Wait for the receive thread to finish
        _recvThread.join();

    close(_socketFd);
}

bool NetworkClient::receivePacket(Packet& packet) {
    char buffer[1024];
    ssize_t len = read(_socketFd, buffer, sizeof(buffer) - 1); // Read data from the socket
    
    if (len <= 0)
        exit(0);

    buffer[len] = '\0';

    try {
        packet = Packet::parse(std::string(buffer)); // Parse the received data into a Packet object
    } catch (const std::exception& e) {
        throw NetworkError("Invalid packet format: " + std::string(e.what()));
    }

    return true;
}

void NetworkClient::sendPacket(const Packet& packet) {
    std::string msg = packet.serialize() + "\n";
    ssize_t sent = SocketActions::writeSocket(_socketFd, msg.c_str(), msg.size()); // Send data to the server
    if (sent < 0)
        throw NetworkError("send() failed");

    if (_debug)
        std::cout << "[SEND] " << msg;
}

bool NetworkClient::getNextPacket(Packet& packet) {
    std::lock_guard<std::mutex> lock(_inboxMutex); // Lock the inbox mutex to access the inbox queue

    if (_inbox.empty())
        return false;

    packet = _inbox.front(); // Get the front packet from the inbox
    _inbox.pop();

    return true;
}

void NetworkClient::receiveLoop() {
    char buffer[1024];
    while (_running) {
        ssize_t len = read(_socketFd, buffer, sizeof(buffer) - 1); // Read data from the socket
        if (len <= 0) break;

        buffer[len] = '\0';
        std::istringstream ss(buffer); // Create a string stream from the buffer
        std::string line;
        while (std::getline(ss, line)) { // Read lines from the string stream
            try {
                Packet pkt = Packet::parse(line); // Parse the line into a Packet object

                std::lock_guard<std::mutex> lock(_inboxMutex); // Lock the inbox mutex to access the inbox queue
                _inbox.push(pkt); // Push the packet into the inbox queue
            } catch (...) {
                if (_debug)
                    std::cerr << "[WARN] Invalid packet: " << line << std::endl;
            }
        }
    }
}

int NetworkClient::getSocketFd() const {
    return _socketFd;
}
