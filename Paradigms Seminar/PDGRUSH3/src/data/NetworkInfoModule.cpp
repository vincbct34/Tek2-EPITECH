/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Network Info Module
*/

#include "../../include/NetworkInfoModule.hpp"

#include <ncurses.h>
#include <fstream>
#include <sstream>

NetworkInfoModule::NetworkInfoModule()
    : IModule(IModule::Mode::Performance, IModule::DataType::Graphical, "Network Info")
{
    update();
}

void NetworkInfoModule::update()
{
    updateNetworkUsage();
}

void NetworkInfoModule::updateNetworkUsage()
{
    std::ifstream file("/proc/net/dev");
    std::string line;
    std::string interface;
    long rx, tx;

    rxBytes = 0;
    txBytes = 0;

    while (std::getline(file, line)) {
        std::istringstream iss(line);
        if (line.find(":") != std::string::npos) {
            iss >> interface >> rx;
            for (int i = 0; i < 7; ++i) iss >> tx; 
            rxBytes += rx;
            txBytes += tx;
        }
    }

    if (prevRxBytes != 0 && prevTxBytes != 0) {
        rxBytes -= prevRxBytes;
        txBytes -= prevTxBytes;
    }

    prevRxBytes = rxBytes;
    prevTxBytes = txBytes;
}

std::string NetworkInfoModule::getData() const
{
    return "RX: " + std::to_string(rxBytes) + " bytes | TX: " + std::to_string(txBytes) + " bytes";
}
