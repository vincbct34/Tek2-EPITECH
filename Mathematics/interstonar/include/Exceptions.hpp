/*
** EPITECH PROJECT, 2025
** Interstonar
** File description:
** Exceptions
*/

#pragma once

#include <string>

class Exceptions {
    public:
        Exceptions(const std::string &message);
        const char *what() const noexcept;

    private:
        std::string _message;
};

class InvalidArguments : public Exceptions {
    public:
        InvalidArguments(const std::string &message);
};

class ParseException : public Exceptions {
    public:
        ParseException(const std::string &message);
};

class FileNotFound : public Exceptions {
    public:
        FileNotFound(const std::string &message);
};
