/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** exceptions
*/

#pragma once

#include <stdexcept>
#include <string>

// Base exception class
class Exception : public std::runtime_error {
public:
  explicit Exception(const std::string &message)
      : std::runtime_error(message) {}
};

// Args error
class ArgsException : public Exception {
public:
  explicit ArgsException(const std::string &message)
      : Exception("Args Error: " + message) {}
};

// Network error
class NetworkException : public Exception {
public:
  explicit NetworkException(const std::string &message)
      : Exception("Network Error: " + message) {}
};
