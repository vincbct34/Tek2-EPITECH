/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** Exceptions
*/

#pragma once

#include <stdexcept>
#include <string>

class Exception : public std::runtime_error {
public:
    explicit Exception(const std::string& message)
        : std::runtime_error("Stone Analysis: " + message) {}
};

class ArgumentsError : public Exception {
public:
    explicit ArgumentsError(const std::string& message)
        : Exception("Arguments Error: " + message) {}
};

class FeatureError : public Exception {
public:
    explicit FeatureError(const std::string& message)
        : Exception("Feature Error: " + message) {}
};