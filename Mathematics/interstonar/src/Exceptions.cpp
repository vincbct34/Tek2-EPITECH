/*
** EPITECH PROJECT, 2025
** Interstonar
** File description:
** Exceptions
*/

#include "Exceptions.hpp"

Exceptions::Exceptions(const std::string &message) : _message(message) {}

const char *Exceptions::what() const noexcept {
    return _message.c_str();
}

InvalidArguments::InvalidArguments(const std::string &message) : Exceptions(message) {}

ParseException::ParseException(const std::string &message) : Exceptions(message) {}

FileNotFound::FileNotFound(const std::string &message) : Exceptions(message) {}
