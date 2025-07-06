/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** Exceptions.cpp
*/

#include "Exceptions.hpp"

ServerError::ServerError(const std::string& message) : _message("Server error: " + message) {}

DirectoryHandlingError::DirectoryHandlingError(const std::string& message) : _message("Directory handling error: " + message) {}

PollError::PollError(const std::string& message) : _message("Polling error: " + message) {}

WriteError::WriteError(const std::string& message) : _message("Write error: " + message) {}