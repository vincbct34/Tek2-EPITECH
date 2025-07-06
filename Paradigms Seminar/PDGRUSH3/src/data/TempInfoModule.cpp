/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3 - Temperature and Fan Speed Monitor
*/

#include "../../include/TempInfoModule.hpp"
#include <fstream>
#include <string>
#include <vector>

TempFanModule::TempFanModule()
    : IModule(IModule::Mode::Performance, IModule::DataType::Text, "Temperature and Fan Speed")
{
    update();
}

void TempFanModule::update()
{
    globalTemperature.clear();
    fanSpeeds.clear();
    readGlobalTemperature();;
}

void TempFanModule::readGlobalTemperature() 
{
    std::ifstream file("/sys/class/thermal/thermal_zone0/temp");
    if (file) {
        std::string value;
        file >> value;
        globalTemperature = "Global Temp: " + std::to_string(std::stoi(value) / 1000) + "°C";
    }
}

std::string TempFanModule::getData() const
{
    std::string data = globalTemperature;
    return data;
}