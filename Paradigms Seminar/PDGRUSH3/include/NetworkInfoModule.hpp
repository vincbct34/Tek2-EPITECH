/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Network Info Module
*/

#pragma once

#include "IModule.hpp"
#include <string>
#include <SFML/Graphics.hpp>

class NetworkInfoModule : public IModule {
public:
    NetworkInfoModule();

    void update() override;
    
    long getRxBytes() const { return rxBytes; }
    long getTxBytes() const { return txBytes; }
    std::string getData() const override;

private:
    void updateNetworkUsage();

    std::string _name = "NetworkInfoModule";
    long rxBytes;
    long txBytes;
    long prevRxBytes = 0;
    long prevTxBytes = 0;
};
