/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#include "../../include/HostnameUserModule.hpp"

#include <SFML/Graphics.hpp>
#include <ncurses.h>

HostnameUserModule::HostnameUserModule()
    : IModule(IModule::Mode::Performance, IModule::DataType::Text, "Hostname and User")
{
    update();
}

void HostnameUserModule::update() {
    char host[256];
    if (gethostname(host, sizeof(host)) == 0) {
        hostname = host;
    } else {
        hostname = "Unknown";
    }
    
    struct passwd* pw = getpwuid(getuid());
    if (pw) {
        username = pw->pw_name;
    } else {
        username = "Unknown";
    }
}

std::string HostnameUserModule::getData() const {
    return "Hostname: " + hostname + " | User: " + username;
}
