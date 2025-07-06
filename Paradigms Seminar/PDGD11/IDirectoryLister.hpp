/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - Listing directories
*/

#pragma once

#include <string>
#include <exception>

class IDirectoryLister {
    public:
        virtual ~IDirectoryLister() = default;

        virtual bool open(const std::string &path, bool hidden) = 0;
        virtual std::string get() = 0;

    class OpenFailureException : public std::exception {
        public:
            const char * what() const noexcept override;
    };

    class NoMoreFileException : public std::exception {
        public:
            const char * what() const noexcept override;
    };
};
