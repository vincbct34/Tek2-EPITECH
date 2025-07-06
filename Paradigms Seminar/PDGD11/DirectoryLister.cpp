/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - Listing directories
*/

#include "DirectoryLister.hpp"

#include <cstdio>
#include <dirent.h>

DirectoryLister::DirectoryLister()
    : _dir(nullptr), _hidden(false)
{
}

DirectoryLister::DirectoryLister(const std::string &path, bool hidden)
    : _dir(nullptr), _hidden(hidden)
{
    open(path, hidden);
}

DirectoryLister::~DirectoryLister()
{
    if (this->_dir != nullptr)
        closedir(this->_dir);
}

bool DirectoryLister::open(const std::string &path, bool hidden)
{
    if (this->_dir != nullptr) {
        closedir(this->_dir);
        this->_dir = nullptr;
    }

    this->_dir = opendir(path.c_str());

    if (this->_dir == nullptr) {
        perror(path.c_str());
        return false;
    }

    this->_hidden = hidden;

    return true;
}

std::string DirectoryLister::get()
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

    return "";
}
