/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1 - Error management
*/

#include "IDirectoryLister.hpp"

#include <cstring>

const char *IDirectoryLister::OpenFailureException::what() const noexcept
{
    return strerror(errno);
}

const char *IDirectoryLister::NoMoreFileException::what() const noexcept
{
    return "End of stream";
}
