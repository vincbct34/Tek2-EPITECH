/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Process Monitoring Module
*/

#pragma once

#include "IModule.hpp"
#include <string>
#include <vector>
#include <SFML/Graphics.hpp>

class ProcessModule : public IModule {
public:
    ProcessModule();

    void update() override;

    const std::vector<std::string>& getProcesses() const { return processes; }
    std::string getData() const override;

private:
    void readProcesses();

    std::string _name = "ProcessModule";
    std::vector<std::string> processes;
};
