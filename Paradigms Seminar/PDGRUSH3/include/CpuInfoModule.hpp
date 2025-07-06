/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** CPU Info Module
*/

#pragma once

#include "IModule.hpp"
#include <string>
#include <SFML/Graphics.hpp>

class CpuInfoModule : public IModule {
public:
    CpuInfoModule();
    void update() override;
    
    const std::string& getModelName() const { return modelName; }
    int getCoreCount() const { return coreCount; }
    double getCpuUsage() const { return cpuUsage; }
    std::string getData() const override;    

private:
    void updateCpuUsage();

    std::string _name = "CpuInfoModule";
    std::string modelName;
    int coreCount;
    double cpuUsage;
    long prevTotal = 0;
    long prevIdle = 0;
};
