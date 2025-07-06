/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#include "include/IManager.hpp"
#include "include/ConsoleDisplay.hpp"
#include "include/HostnameUserModule.hpp"
#include "include/RamUsageModule.hpp"
#include "include/SFMLDisplay.hpp"
#include "include/CpuInfoModule.hpp"
#include "include/NetworkInfoModule.hpp"
#include "include/DateTimeInfoModule.hpp"
#include "include/StockageInfoModule.hpp"
#include "include/TempInfoModule.hpp"
#include "include/ProcessInfoModule.hpp"

#include <iostream>

void dispHelp()
{
    std::cout << "USAGE" << std::endl;
    std::cout << "\t./my_gkrellm -text" << std::endl;
    std::cout << "\t./my_gkrellm -graphical" << std::endl;
    std::cout << "DESCRIPTION" << std::endl;
    std::cout << "\t-text\t\t\t\ttext mode" << std::endl;
    std::cout << "\t-graphical\t\t\tgraphical mode" << std::endl;
}

void addModules(IManager& manager)
{
    HostnameUserModule* hostnameUserModule = new HostnameUserModule();
    manager.addModule(hostnameUserModule);

    RamUsageModule* ramUsageModule = new RamUsageModule();
    manager.addModule(ramUsageModule);

    CpuInfoModule* cpuInfoModule = new CpuInfoModule();
    manager.addModule(cpuInfoModule);

    NetworkInfoModule* networkInfoModule = new NetworkInfoModule();
    manager.addModule(networkInfoModule);

    DateTimeModule* dateTimeModule = new DateTimeModule();
    manager.addModule(dateTimeModule);

    StorageModule* storageModule = new StorageModule();
    manager.addModule(storageModule);

    TempFanModule* tempFanModule = new TempFanModule();
    manager.addModule(tempFanModule);

    ProcessModule* processInfoModule = new ProcessModule();
    manager.addModule(processInfoModule);

    ProcessModule* processModule = new ProcessModule();
    manager.addModule(processModule);
}

int main(int argc, char* argv[]) {
    IManager manager;

    addModules(manager);

    std::string mode = (argc > 1) ? argv[1] : "-help";

    if (mode == "-graphical") {
        SFMLDisplay display;
        manager.updateModules();
        display.render(manager.getModules());
    } else if (mode == "-text") {
        ConsoleDisplay display;
        manager.updateModules();
        display.render(manager.getModules());
    } else
        dispHelp();

    return 0;
}
