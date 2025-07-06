/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#include "../../include/RamUsageModule.hpp"

#include <ncurses.h>
#include <fstream>

RamUsageModule::RamUsageModule()
    : IModule(IModule::Mode::Performance, IModule::DataType::Graphical, "RAM Usage")
{
    update();
}

void RamUsageModule::update()
{
    std::ifstream file("/proc/meminfo");

    if (!file.is_open())
        return;

    std::string line;
    while (std::getline(file, line)) {
        if (line.find("MemTotal:") != std::string::npos)
            totalRam = std::stoul(line.substr(10));
        else if (line.find("MemFree:") != std::string::npos)
            freeRam = std::stoul(line.substr(9));
    }

    usedRam = totalRam - freeRam;
}

std::string RamUsageModule::getData() const
{
    return "Total RAM: " + std::to_string(totalRam) + " | Used RAM: " + std::to_string(usedRam) + " | Free RAM: " + std::to_string(freeRam);
}

// Module de monitoring de l'utilisation de la RAM
