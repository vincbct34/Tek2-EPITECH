/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#include "../../include/SFMLDisplay.hpp"
#include <sstream>

void SFMLDisplay::performanceStyle(sf::RenderWindow& window, const std::string& data, IModule* module, float& xOffset, float& yOffset, float moduleWidth, float moduleHeight, sf::Font& font)
{
    sf::RectangleShape moduleBackground(sf::Vector2f(moduleWidth, moduleHeight));
    moduleBackground.setFillColor(sf::Color(0, 0, 0, 180));
    moduleBackground.setOutlineColor(sf::Color(255, 165, 0));
    moduleBackground.setOutlineThickness(3);
    moduleBackground.setPosition(xOffset, yOffset);

    sf::Text moduleTitle;
    moduleTitle.setFont(font);
    moduleTitle.setString(module->getName());
    moduleTitle.setCharacterSize(24);
    moduleTitle.setFillColor(sf::Color::White);
    sf::FloatRect titleRect = moduleTitle.getLocalBounds();
    moduleTitle.setOrigin(titleRect.width / 2.0f, titleRect.height / 2.0f);
    moduleTitle.setPosition(xOffset + moduleWidth / 2.0f, yOffset + 30);

    window.draw(moduleBackground);
    window.draw(moduleTitle);

    if (module->dataType == IModule::DataType::Text) {
        sf::Text moduleText;
        moduleText.setFont(font);
        moduleText.setCharacterSize(20);
        moduleText.setFillColor(sf::Color::White);

        std::string formattedText;
        std::istringstream iss(data);
        std::string word;
        float maxWidth = moduleWidth - 20;
        std::string line;
        std::vector<std::string> lines;

        while (iss >> word) {
            sf::Text tempText(line + " " + word, font, 20);
            if (tempText.getLocalBounds().width > maxWidth) {
                lines.push_back(line);
                line = word;
            } else {
                if (!line.empty()) line += " ";
                line += word;
            }
        }
        lines.push_back(line);

        for (const auto& l : lines)
            formattedText += l + "\n";

        moduleText.setString(formattedText);
        sf::FloatRect textRect = moduleText.getLocalBounds();
        moduleText.setOrigin(textRect.width / 2.0f, textRect.height / 2.0f);
        moduleText.setPosition(xOffset + moduleWidth / 2.0f, yOffset + moduleHeight / 2.0f);

        window.draw(moduleBackground);
        window.draw(moduleTitle);
        window.draw(moduleText);
    }

    if (module->dataType == IModule::DataType::Graphical) {
        if (module->getName() == "RAM Usage") {
            float totalRam, usedRam, freeRam;
            std::sscanf(data.c_str(), "Total RAM: %f | Used RAM: %f | Free RAM: %f", &totalRam, &usedRam, &freeRam);

            static std::vector<float> usedRamHistory;
            static const size_t maxHistorySize = 50;

            usedRamHistory.push_back(usedRam);
            if (usedRamHistory.size() > maxHistorySize) usedRamHistory.erase(usedRamHistory.begin());

            float graphWidth = moduleWidth - 40;
            float graphHeight = moduleHeight / 3;
            float graphX = xOffset + 20;
            float graphY = yOffset + moduleHeight - graphHeight - 20;

            sf::RectangleShape graphBackground(sf::Vector2f(graphWidth, graphHeight));
            graphBackground.setFillColor(sf::Color(50, 50, 50, 180));
            graphBackground.setPosition(graphX, graphY);

            window.draw(graphBackground);

            float barWidth = graphWidth / maxHistorySize;
            for (size_t i = 0; i < usedRamHistory.size(); ++i) {
                float barHeight = (usedRamHistory[i] / totalRam) * graphHeight;
                sf::RectangleShape bar(sf::Vector2f(barWidth - 2, barHeight));
                bar.setFillColor(sf::Color(255, 0, 0, 200));
                bar.setPosition(graphX + i * barWidth, graphY + graphHeight - barHeight);
                window.draw(bar);
            }

            sf::Text usedLabel;
            usedLabel.setFont(font);
            usedLabel.setString("Used RAM: " + std::to_string(static_cast<float>(usedRam / 1000000)) + " GB");
            usedLabel.setCharacterSize(14);
            usedLabel.setFillColor(sf::Color::White);
            usedLabel.setPosition(graphX, graphY - 20);

            sf::Text freeLabel;
            freeLabel.setFont(font);
            freeLabel.setString("Free RAM: " + std::to_string(static_cast<float>(freeRam / 1000000)) + " GB");
            freeLabel.setCharacterSize(14);
            freeLabel.setFillColor(sf::Color::White);
            freeLabel.setPosition(graphX + graphWidth / 2, graphY - 20);

            window.draw(usedLabel);
            window.draw(freeLabel);
        } else if (module->getName() == "Network Info") {
            float rxBytes, txBytes;
            std::sscanf(data.c_str(), "RX: %f bytes | TX: %f bytes", &rxBytes, &txBytes);

            float graphWidth = moduleWidth - 40;
            float graphHeight = moduleHeight / 3;
            float graphX = xOffset + 20;
            float graphY = yOffset + moduleHeight - graphHeight - 20;

            sf::RectangleShape graphBackground(sf::Vector2f(graphWidth, graphHeight));
            graphBackground.setFillColor(sf::Color(50, 50, 50, 180));
            graphBackground.setPosition(graphX, graphY);

            window.draw(graphBackground);

            float maxBytes = std::max(rxBytes, txBytes);
            float rxBarHeight = (rxBytes / maxBytes) * graphHeight;
            float txBarHeight = (txBytes / maxBytes) * graphHeight;

            sf::RectangleShape rxBar(sf::Vector2f(graphWidth / 2 - 10, rxBarHeight));
            rxBar.setFillColor(sf::Color(0, 255, 0, 200));
            rxBar.setPosition(graphX, graphY + graphHeight - rxBarHeight);

            sf::RectangleShape txBar(sf::Vector2f(graphWidth / 2 - 10, txBarHeight));
            txBar.setFillColor(sf::Color(0, 0, 255, 200));
            txBar.setPosition(graphX + graphWidth / 2 + 10, graphY + graphHeight - txBarHeight);

            window.draw(rxBar);
            window.draw(txBar);

            sf::Text rxLabel;
            rxLabel.setFont(font);
            rxLabel.setString("RX: " + std::to_string(static_cast<float>(rxBytes / 1000000)) + " MB");
            rxLabel.setCharacterSize(14);
            rxLabel.setFillColor(sf::Color::White);
            rxLabel.setPosition(graphX, graphY - 20);

            sf::Text txLabel;
            txLabel.setFont(font);
            txLabel.setString("TX: " + std::to_string(static_cast<float>(txBytes / 1000000)) + " MB");
            txLabel.setCharacterSize(14);
            txLabel.setFillColor(sf::Color::White);
            txLabel.setPosition(graphX + graphWidth / 2 + 10, graphY - 20);

            window.draw(rxLabel);
            window.draw(txLabel);
        } else if (module->getName() == "Infos CPU") {
            std::string modelName;
            int coreCount;
            float cpuUsage;

            size_t modelPos = data.find("Model: ");
            size_t coresPos = data.find(" | Cores: ");
            size_t usagePos = data.find(" | Usage: ");

            if (modelPos != std::string::npos && coresPos != std::string::npos && usagePos != std::string::npos) {
                modelName = data.substr(modelPos + 7, coresPos - (modelPos + 7));
                coreCount = std::stoi(data.substr(coresPos + 9, usagePos - (coresPos + 9)));
                cpuUsage = std::stof(data.substr(usagePos + 10));
            }

            static std::vector<float> cpuUsageHistory;
            static const size_t maxHistorySize = 50;

            cpuUsageHistory.push_back(cpuUsage);
            if (cpuUsageHistory.size() > maxHistorySize) cpuUsageHistory.erase(cpuUsageHistory.begin());

            float graphWidth = moduleWidth - 40;
            float graphHeight = moduleHeight / 3;
            float graphX = xOffset + 20;
            float graphY = yOffset + moduleHeight - graphHeight - 20;

            sf::RectangleShape graphBackground(sf::Vector2f(graphWidth, graphHeight));
            graphBackground.setFillColor(sf::Color(50, 50, 50, 180));
            graphBackground.setPosition(graphX, graphY);

            window.draw(graphBackground);

            float barWidth = graphWidth / maxHistorySize;
            for (size_t i = 0; i < cpuUsageHistory.size(); ++i) {
                float barHeight = (cpuUsageHistory[i] / 100.0f) * graphHeight;
                sf::RectangleShape bar(sf::Vector2f(barWidth - 2, barHeight));
                bar.setFillColor(sf::Color(0, 255, 0, 200));
                bar.setPosition(graphX + i * barWidth, graphY + graphHeight - barHeight);
                window.draw(bar);
            }

            sf::Text modelLabel;
            modelLabel.setFont(font);
            modelLabel.setString("Model: " + modelName);
            modelLabel.setCharacterSize(14);
            modelLabel.setFillColor(sf::Color::White);
            modelLabel.setPosition(graphX, graphY - 40);

            sf::Text coresLabel;
            coresLabel.setFont(font);
            coresLabel.setString("Cores: " + std::to_string(coreCount));
            coresLabel.setCharacterSize(14);
            coresLabel.setFillColor(sf::Color::White);
            coresLabel.setPosition(graphX, graphY - 20);

            sf::Text usageLabel;
            usageLabel.setFont(font);
            usageLabel.setString("Usage: " + std::to_string(static_cast<int>(cpuUsage)) + "%");
            usageLabel.setCharacterSize(14);
            usageLabel.setFillColor(sf::Color::White);
            usageLabel.setPosition(graphX + graphWidth / 2, graphY - 20);

            window.draw(modelLabel);
            window.draw(coresLabel);
            window.draw(usageLabel);
        }
    }
}
