/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** DlEncapsulation
*/

#pragma once

#include <dlfcn.h>
#include <string>

class DlEncapsulation {
public:
    static void *openLib(std::string const &path) {
        return dlopen(path.c_str(), RTLD_LAZY);
    }

    static void closeLib(void *handle) {
        if (handle) {
            dlclose(handle);
        }
    }

    static void *getLib(void *handle, const char *symbol) {
        return dlsym(handle, symbol);
    }

    static const std::string errorLib() {
        return dlerror();
    }
};