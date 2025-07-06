/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** Exceptions.cpp
*/

#include "Exceptions.hpp"

FileError::FileError(const std::string &message) : _message(message) {}

const char* FileError::what() const noexcept {
    return _message.c_str();
}

ComponentTypeError::ComponentTypeError(const std::string &type) : _message("Unknown component type: " + type) {}

const char* ComponentTypeError::what() const noexcept {
    return _message.c_str();
}

ComponentNameError::ComponentNameError(const std::string &name) : _message("Unknown component name: " + name) {}

const char* ComponentNameError::what() const noexcept {
    return _message.c_str();
}

DuplicateComponentError::DuplicateComponentError(const std::string &name) : _message("Duplicate component name: " + name) {}

const char* DuplicateComponentError::what() const noexcept {
    return _message.c_str();
}

NoChipsetsError::NoChipsetsError() : _message("Error: No chipsets added to the circuit.") {}

const char* NoChipsetsError::what() const noexcept {
    return _message.c_str();
}

BadArgumentsError::BadArgumentsError() : _message("Bad arguments\nUsage: ./nanotekspice [filename]") {}

const char* BadArgumentsError::what() const noexcept {
    return _message.c_str();
}

NoLinksError::NoLinksError() : _message("Error: No links added to the circuit.") {}

const char* NoLinksError::what() const noexcept {
    return _message.c_str();
}

SyntaxicError::SyntaxicError(const std::string &message) : _message("Error: Syntaxic error, " + message) {}
    
const char* SyntaxicError::what() const noexcept {
    return _message.c_str();
}
