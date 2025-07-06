/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - Listing directories
*/

#pragma once

#include "IDirectoryLister.hpp"

#include <string>
#include <dirent.h>

class DirectoryLister : public IDirectoryLister {
    public:
        DirectoryLister();
        DirectoryLister(const std::string &path, bool hidden);
        ~DirectoryLister();

        bool open(const std::string &path, bool hidden) override;
        std::string get() override;

    private:
        DIR *_dir;
        bool _hidden;
        std::string _path;
};
