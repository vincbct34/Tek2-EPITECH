/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** CPU Info Module
*/

#include "../../include/CpuInfoModule.hpp"

#include <ncurses.h>
#include <fstream>
#include <sstream>

CpuInfoModule::CpuInfoModule()
    : IModule(IModule::Mode::Performance, IModule::DataType::Graphical, "Infos CPU")
{
    update();
}

void CpuInfoModule::update()
{
    std::ifstream file("/proc/cpuinfo");
    std::string line;

    modelName = "Unknown";
    coreCount = 0;

    while (std::getline(file, line)) {
        if (line.find("model name") != std::string::npos) {
            modelName = line.substr(line.find(":") + 2);
        }
        if (line.find("processor") != std::string::npos) {
            coreCount++;
        }
    }
    file.close();

    updateCpuUsage();
}

void CpuInfoModule::updateCpuUsage()
{
    std::ifstream file("/proc/stat");
    std::string line;
    if (std::getline(file, line)) {
        std::istringstream iss(line);
        std::string cpu;
        long user, nice, system, idle;
        iss >> cpu >> user >> nice >> system >> idle;
        long total = user + nice + system + idle;
        
        if (prevTotal != 0) {
            long totalDiff = total - prevTotal;
            long idleDiff = idle - prevIdle;
            cpuUsage = (1.0 - ((double)idleDiff / totalDiff)) * 100.0;
        }
        
        prevTotal = total;
        prevIdle = idle;
    }
}

std::string CpuInfoModule::getData() const
{
    return "Model: " + modelName + " | Cores: " + std::to_string(coreCount) + " | Usage: " + std::to_string(cpuUsage) + "%";
}
