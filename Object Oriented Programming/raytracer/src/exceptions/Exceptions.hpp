/*
** EPITECH PROJECT, 2025
** B-OOP-400-MPL-4-1-raytracer-vincent.bichat
** File description:
** exceptions
*/

#pragma once

#include <stdexcept>
#include <string>

// Base exception class
class Exception : public std::runtime_error {
public:
    explicit Exception(const std::string& message)
        : std::runtime_error(message) {}
};

// Parsing error
class ParsingException : public Exception {
public:
    explicit ParsingException(const std::string& message)
        : Exception("Parsing Error: " + message) {}
};

// RayTracer error
class RayTracerException : public Exception {
public:
    explicit RayTracerException(const std::string& message)
        : Exception("RayTracer Error: " + message) {}
};
