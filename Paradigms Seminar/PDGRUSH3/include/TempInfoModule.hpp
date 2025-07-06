/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Temperature and Fan Speed Module
*/

#pragma once

#include "IModule.hpp"
#include <string>
#include <vector>

class TempFanModule : public IModule {
public:
    TempFanModule();

    void update() override;

    const std::string& getGlobalTemperature() const { return globalTemperature; }
    std::string getData() const override;

private:
    void readGlobalTemperature();

    std::string _name = "TempFanModule";
    std::string globalTemperature;
    std::vector<std::string> fanSpeeds;
};
