/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/


#include <iostream>
#include "../../include/SFMLDisplay.hpp"
#include "../../include/IManager.hpp"
#include <iostream>

SFMLDisplay::SFMLDisplay()
    : window(sf::VideoMode(1920, 1080), "MyGKrellm - SFML Mode", sf::Style::Close)
{
    if (!font.loadFromFile("./assets/fonts/Montserrat-Regular.ttf"))
        std::cerr << "Failed to load font \"Montserrat-Regular.ttf\"" << std::endl;

    // Welcome buttons
    int middle = 1920 / 2;
    int buttonWidth = 150;
    int buttonSpacing = 20;
    int totalWidth = (buttonWidth * 2) + buttonSpacing;
    int startX = middle - (totalWidth / 2);

    welcomeButtons.emplace_back(startX, 550, buttonWidth, 40, "Performance", "perf_display", font);
    startX += buttonWidth + buttonSpacing;
    welcomeButtons.emplace_back(startX, 550, buttonWidth, 40, "Task Manager", "task_manager", font);

    startX = middle - (totalWidth / 2);
    
    // Top buttons

    modeButtons.emplace_back(startX, 10, buttonWidth, 40, "Task Manager", "process_display", font);
    startX += buttonWidth + buttonSpacing;
    modeButtons.emplace_back(startX, 10, buttonWidth, 40, "Performance", "perf_display", font);

    startX = middle - (totalWidth / 2);

    // Sort buttons
    int pos_x = 10;
    totalWidth = 128 + 512 + 128 + 256 + 192 + 256;
    float scaleFactor = static_cast<float>(1920 - 20) / totalWidth;

    sortButtons.emplace_back(pos_x, 75, 128 * scaleFactor, 40, "PID", "cpu_action", font);
    pos_x += 128 * scaleFactor;
    sortButtons.emplace_back(pos_x, 75, 512 * scaleFactor, 40, "Process Name", "cpu_action", font);
    pos_x += 512 * scaleFactor;
    sortButtons.emplace_back(pos_x, 75, 128 * scaleFactor, 40, "CPU (%)", "cpu_action", font);
    pos_x += 128 * scaleFactor;
    sortButtons.emplace_back(pos_x, 75, 256 * scaleFactor, 40, "Mem (Mo)", "cpu_action", font);
    pos_x += 256 * scaleFactor;
    sortButtons.emplace_back(pos_x, 75, 192 * scaleFactor, 40, "Net ()", "cpu_action", font);
    pos_x += 192 * scaleFactor;
    sortButtons.emplace_back(pos_x, 75, 256 * scaleFactor, 40, "Disk (Mo)", "cpu_action", font);
}

void SFMLDisplay::render(const std::vector<IModule*>& modules) {
    IManager manager;

    sf::Time frameTime = sf::milliseconds(200);
    sf::Time elapsed;
    sf::Clock clock;

    // Gradient background setup
    backgroundTexture.create(1920, 1080);
    sf::Image bgImage;
    bgImage.create(1920, 1080, sf::Color(20, 20, 30));
    for (unsigned int y = 0; y < 1080; y++) {
        sf::Uint8 shade = static_cast<sf::Uint8>(30 + (y * 50 / 1080));
        for (unsigned int x = 0; x < 1920; x++) {
            bgImage.setPixel(x, y, sf::Color(shade, shade, shade + 20));
        }
    }
    backgroundTexture.update(bgImage);
    backgroundSprite.setTexture(backgroundTexture);

    while (window.isOpen()) {
        elapsed = clock.restart();

        eventsManager(modules, manager);

        float yOffset = 355.0f;
        float yOffsetModules = 150.0f;

        window.clear();
        window.draw(backgroundSprite);

        // Mode Buttons
        if (manager.getMode() != IModule::Mode::WelcomePage) {
            for (auto& button : modeButtons)
                button.draw(window);
        }

        // Affichage des modules
        if (manager.getMode() == IModule::Mode::WelcomePage) {
            sf::Text title;
            title.setFont(font);
            title.setString("Welcome to MyGKrellm");
            title.setCharacterSize(50);
            title.setFillColor(sf::Color(255, 165, 0));
            title.setStyle(sf::Text::Bold);
            sf::FloatRect titleRect = title.getLocalBounds();
            title.setOrigin(titleRect.width / 2.0f, titleRect.height / 2.0f);
            title.setPosition(sf::Vector2f(window.getSize().x / 2.0f, yOffset));

            sf::RectangleShape background(sf::Vector2f(800, 400));
            background.setFillColor(sf::Color(0, 0, 0));
            background.setPosition((window.getSize().x - background.getSize().x) / 2, yOffset - 60);
            background.setOutlineColor(sf::Color(255, 165, 0));
            background.setOutlineThickness(3);

            window.draw(background);
            window.draw(title);
            yOffset += 100.0f;

            sf::Text modeText;
            modeText.setFont(font);
            modeText.setString("Select a Mode:");
            modeText.setCharacterSize(30);
            modeText.setFillColor(sf::Color::White);
            modeText.setStyle(sf::Text::Italic);
            sf::FloatRect modeTextRect = modeText.getLocalBounds();
            modeText.setOrigin(modeTextRect.width / 2.0f, modeTextRect.height / 2.0f);
            modeText.setPosition(sf::Vector2f(window.getSize().x / 2.0f, yOffset));

            window.draw(modeText);

            for (auto& button : welcomeButtons)
                button.draw(window);

        } else if (manager.getMode() == IModule::Mode::Performance) {
            int index = 0;
            int columns = 3;
            float moduleWidth = 600;
            float moduleHeight = 200;
            float padding = 20;

            for (const auto& module : modules) {
                if (module->mode == IModule::Mode::Performance) {
                    module->update();
                    float x = (index % columns) * (moduleWidth + padding) + 50;
                    float y = (index / columns) * (moduleHeight + padding) + 100;
                    
                    sf::RectangleShape moduleBackground(sf::Vector2f(moduleWidth, moduleHeight));
                    moduleBackground.setFillColor(sf::Color(0, 0, 0, 180));
                    moduleBackground.setOutlineColor(sf::Color(255, 165, 0));
                    moduleBackground.setOutlineThickness(3);
                    moduleBackground.setPosition(x, y);
                    
                    window.draw(moduleBackground);
                    performanceStyle(window, module->getData(), module, x, y, moduleWidth, moduleHeight, font);
                    
                    index++;
                }
            }
        } else if (manager.getMode() == IModule::Mode::TaskManager) {
            
            // Sort buttons
            for (auto& button : sortButtons)
                button.draw(window);
            TaskManagerStyle(modules, yOffsetModules);
            for (auto& row : taskRows) {
                row.draw(window);
            }
        }

        window.display();

        if (elapsed < frameTime)
            sf::sleep(frameTime - elapsed);
    }
}
