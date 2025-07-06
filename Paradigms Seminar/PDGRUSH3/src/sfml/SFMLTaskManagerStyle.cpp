/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/


#include <iostream>
#include <sstream>
#include "../../include/SFMLDisplay.hpp"
#include "../../include/IManager.hpp"
#include <iostream>
#include <regex>
#include <cmath>

void SFMLDisplay::TaskManagerStyle(const std::vector<IModule*>& modules, float yOffsetModules) {
    taskRows.clear();  // Clear les anciennes lignes

    std::regex pattern(R"(PID: (\d+) \| Name: \((.*?)\) \| CPU: (\d+) (\d+) \| Mem: (\d+) \| Net: (.*?) \| Disk: (.*))");

    // Calcul des largeurs des colonnes
    int totalWidth = 128 + 512 + 128 + 256 + 192 + 256;
    float scaleFactor = static_cast<float>(1920 - 20) / totalWidth;

    std::vector<float> columnWidths = {
        128 * scaleFactor, // PID
        512 * scaleFactor, // Process Name
        128 * scaleFactor, // CPU (%)
        256 * scaleFactor, // Mem (Mo)
        192 * scaleFactor, // Net
        256 * scaleFactor  // Disk
    };

    for (const auto& module : modules) {
        if (module->mode == IModule::Mode::TaskManager) {
            module->update();
            std::string txt = module->getData();

            std::istringstream stream(txt);
            std::string line;
            while (std::getline(stream, line)) {
                std::smatch match;
                if (std::regex_match(line, match, pattern)) {
                    int moConvertedForMem = std::round(std::stof(match[5].str()) / 1000);
                    int moConvertedForDisk = 0;

                    if (match[7].str() != "N/A")
                        moConvertedForDisk = std::round(std::stof(std::regex_replace(match[7].str(), std::regex("cancelled_write_bytes:"), "")) / 1000000);

                    std::vector<std::string> fields = {
                        match[1], // PID
                        match[2], // Process Name
                        match[3].str() + " / " + match[4].str(), // CPU (%)
                        std::to_string(moConvertedForMem), // Mem (Mo)
                        (match[6].str() == "N/A" ? "0" : match[6].str()), // Net
                        std::to_string(moConvertedForDisk) // Disk
                    };

                    float pos_x = 10;
                    float pos_y = yOffsetModules - scrollOffset;
                    if (pos_y >= 150) {
                        for (size_t i = 0; i < fields.size(); ++i) {
                            taskRows.emplace_back(pos_y, pos_x, columnWidths[i], fields[i], font);
                            pos_x += columnWidths[i];
                        }
                    }

                    yOffsetModules += 45;
                }
            }
        }
    }
}

void SFMLDisplay::setScrollOffset(float offset) {
    scrollOffset = offset;
}

