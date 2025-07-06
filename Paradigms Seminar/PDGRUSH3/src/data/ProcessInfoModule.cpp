/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3 - Process Monitoring Module
*/

#include "../../include/ProcessInfoModule.hpp"
#include <fstream>
#include <sstream>
#include <vector>
#include <filesystem>
#include <algorithm>

struct ProcessInfo {
    std::string pid;
    std::string name;
    std::string cpuUsage;
    std::string memoryUsage;
    std::string netUsage;
    std::string diskUsage;
};

ProcessModule::ProcessModule()
    : IModule(IModule::Mode::TaskManager, IModule::DataType::Text, "Process Monitoring")
{
    update();
}

void ProcessModule::update() {
    processes.clear();
    readProcesses();
}

void ProcessModule::readProcesses() {
    std::vector<ProcessInfo> processInfos;

    for (const auto& entry : std::filesystem::directory_iterator("/proc")) {
        if (!entry.is_directory()) continue;

        std::string pid = entry.path().filename();
        if (!std::all_of(pid.begin(), pid.end(), ::isdigit)) continue;

        std::ifstream file(entry.path() / "stat");
        if (!file.is_open()) continue;

        std::string line;
        std::getline(file, line);
        std::istringstream iss(line);
        std::vector<std::string> tokens;
        std::string token;
        while (iss >> token)
            tokens.push_back(token);

        if (tokens.size() < 24) continue;

        std::string name = tokens[1];
        std::string cpuUsage = tokens[13] + " " + tokens[14];
        std::string memoryUsage = tokens[23];
        std::string netUsage = "N/A";
        std::ifstream netFile(entry.path() / "net/dev");
        if (netFile.is_open()) {
            std::string netLine;
            while (std::getline(netFile, netLine)) {
                if (netLine.find("eth0:") != std::string::npos) {
                    std::istringstream netIss(netLine);
                    std::vector<std::string> netTokens;
                    std::string netToken;
                    while (netIss >> netToken)
                        netTokens.push_back(netToken);
                    if (netTokens.size() >= 10)
                        netUsage = "RX: " + netTokens[1] + "B | TX: " + netTokens[9] + "B";
                    break;
                }
            }
        }

        std::string diskUsage = "N/A";
        std::ifstream ioFile(entry.path() / "io");
        if (ioFile.is_open()) {
            std::string ioLine;
            while (std::getline(ioFile, ioLine)) {
                if (ioLine.find("read_bytes:") != std::string::npos || ioLine.find("write_bytes:") != std::string::npos)
                    diskUsage = ioLine + " ";
            }
        }

        processInfos.push_back({pid, name, cpuUsage, memoryUsage, netUsage, diskUsage});
    }

    // Sort the processInfos vector by PID
    std::sort(processInfos.begin(), processInfos.end(), [](const ProcessInfo& a, const ProcessInfo& b) {
        return std::stoi(a.cpuUsage) > std::stoi(b.cpuUsage);
    });

    for (const auto& processInfo : processInfos) {
        processes.push_back("PID: " + processInfo.pid + " | Name: " + processInfo.name + " | CPU: " + processInfo.cpuUsage + " | Mem: " + processInfo.memoryUsage + " | Net: " + processInfo.netUsage + " | Disk: " + processInfo.diskUsage);
    }
}

std::string ProcessModule::getData() const {
    std::string data;
    for (const auto& process : processes)
        data += process + "\n";
    return data;
}
