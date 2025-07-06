/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#include "../../include/SFMLDisplay.hpp"
#include "../../include/IManager.hpp"

void SFMLDisplay::eventsManager(const std::vector<IModule*>& modules, IManager& manager)
{
    (void)modules;
    sf::Event event;

    while (window.pollEvent(event)) {
        if (event.type == sf::Event::Closed) {
            window.close();
        }

        if (event.type == sf::Event::MouseButtonPressed && event.mouseButton.button == sf::Mouse::Left) {
            sf::Vector2i mousePos = sf::Mouse::getPosition(window);
            for (auto& button : welcomeButtons) {
                if (button.isClicked(mousePos)) {
                    if (button.getAction() == "perf_display")
                        manager.setMode(IModule::Mode::Performance);
                    else if (button.getAction() == "task_manager")
                        manager.setMode(IModule::Mode::TaskManager);
                }
            }
            for (auto& button : modeButtons) {
                if (button.isClicked(mousePos)) {
                    if (button.getAction() == "perf_display")
                        manager.setMode(IModule::Mode::Performance);
                    else if (button.getAction() == "process_display")
                        manager.setMode(IModule::Mode::TaskManager);
                }
            }
        }

        // Handle mouse wheel scroll event
        if (event.type == sf::Event::MouseWheelScrolled) {
            if (event.mouseWheelScroll.wheel == sf::Mouse::VerticalWheel) {
                float scrollDelta = -event.mouseWheelScroll.delta * 45;
                float newScrollOffset = scrollOffset + scrollDelta;
                if (newScrollOffset >= 0) {
                    setScrollOffset(newScrollOffset);
                } else {
                    setScrollOffset(0);
                }
            }
        }
    }
}
