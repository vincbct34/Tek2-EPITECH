/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Exceptions
*/

#pragma once

#include <stdexcept>
#include <string>

/**
 * @brief Base class for all Jetpack exceptions.
 */
class JetpackError : public std::runtime_error {
public:
    explicit JetpackError(const std::string& msg)
        : std::runtime_error("[Jetpack] " + msg) {}
};

/**
 * @brief Exception thrown when a server error occurs.
 */
class ServerError : public JetpackError {
public:
    explicit ServerError(const std::string& msg)
        : JetpackError("[Server] " + msg) {}
};

/**
 * @brief Exception thrown when a socket error occurs.
 */
class SocketError : public JetpackError {
public:
    explicit SocketError(const std::string& msg)
        : JetpackError("[Socket] " + msg) {}
};

/**
 * @brief Exception thrown when a client-side error occurs.
 */
class ClientError : public JetpackError {
public:
    explicit ClientError(const std::string& msg)
        : JetpackError("[ClientSession] " + msg) {}
};

/**
 * @brief Exception thrown when a map error occurs.
 */
class MapError : public JetpackError {
public:
    explicit MapError(const std::string& msg)
        : JetpackError("[Map] " + msg) {}
};

/**
 * @brief Exception thrown when a player error occurs.
 */
class NetworkError : public JetpackError {
public:
    explicit NetworkError(const std::string& msg)
        : JetpackError("[Network] " + msg) {}
};

/**
 * @brief Exception thrown when a SFML error occurs.
 */
class SFMLException : public JetpackError {
public:
    explicit SFMLException(const std::string& msg)
        : JetpackError("[SFML] " + msg) {}
};