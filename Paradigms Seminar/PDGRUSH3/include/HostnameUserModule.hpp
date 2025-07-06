/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#pragma once

#include "IModule.hpp"

#include <SFML/Graphics.hpp>
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>
#include <string>

class HostnameUserModule : public IModule {
public:
    HostnameUserModule();

    void update() override;
    std::string getData() const override;

private:
    std::string hostname;
    std::string username;
};
