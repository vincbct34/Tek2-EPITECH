/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** Exceptions.hpp
*/

#pragma once

#include <exception>
#include <string>

class ServerError : public std::exception {
public:
    ServerError(const std::string& message);

    const char* what() const noexcept override {
        return _message.c_str();
    }

private:
    std::string _message;
};

class DirectoryHandlingError : public std::exception {
public:
    DirectoryHandlingError(const std::string& message);

    const char* what() const noexcept override {
        return _message.c_str();
    }

private:
    std::string _message;
};

class PollError : public std::exception {
public:
    PollError(const std::string& message);

    const char* what() const noexcept override {
        return _message.c_str();
    }

private:
    std::string _message;
};

class WriteError : public std::exception {
public:
    WriteError(const std::string& message);

    const char* what() const noexcept override {
        return _message.c_str();
    }

private:
    std::string _message;
};
