/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#pragma once

#include "IModule.hpp"
#include <cstddef>

class RamUsageModule : public IModule {
public:
    RamUsageModule();

    void update() override;
    std::string getData() const override;

private:
    size_t totalRam;
    size_t usedRam;
    size_t freeRam;
};
