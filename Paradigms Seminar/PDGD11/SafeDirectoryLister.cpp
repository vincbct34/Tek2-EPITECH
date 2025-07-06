/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1 - Error management
*/

#include "SafeDirectoryLister.hpp"

SafeDirectoryLister::SafeDirectoryLister()
    : _dir(nullptr), _hidden(false)
{
}

SafeDirectoryLister::SafeDirectoryLister(const std::string &path, bool hidden)
    : _dir(nullptr), _hidden(hidden)
{
    open(path, hidden);
}

SafeDirectoryLister::~SafeDirectoryLister()
{
    if (this->_dir != nullptr)
        closedir(this->_dir);
}

bool SafeDirectoryLister::open(const std::string &path, bool hidden)
{
    if (this->_dir != nullptr) {
        closedir(this->_dir);
        this->_dir = nullptr;
    }

    this->_dir = opendir(path.c_str());

    if (this->_dir == nullptr) {
        throw OpenFailureException();
        return true;
    }

    this->_hidden = hidden;

    return true;
}

std::string SafeDirectoryLister::get()
{
    if (this->_dir == nullptr)
        return "";

    struct dirent *entry;
    while ((entry = readdir(this->_dir)) != nullptr) {
        std::string name = entry->d_name;

        if (!this->_hidden && name[0] == '.')
            continue;

        return name;
    }

    throw NoMoreFileException();

    return "";
}
