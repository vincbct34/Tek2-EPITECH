/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1 - Error management
*/

#pragma once

#include "IDirectoryLister.hpp"

#include <string>
#include <dirent.h>

class SafeDirectoryLister : public IDirectoryLister {
    public:
        SafeDirectoryLister();
        SafeDirectoryLister(const std::string &path, bool hidden);
        ~SafeDirectoryLister();

        bool open(const std::string &path, bool hidden) override;
        std::string get() override;

    private:
        DIR *_dir;
        bool _hidden;
        std::string _path;
};
