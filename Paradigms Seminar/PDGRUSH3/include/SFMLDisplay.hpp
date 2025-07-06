/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#pragma once

#include "IDisplay.hpp"
#include "IManager.hpp"

#include <SFML/Graphics.hpp>
#include <vector>

class Button {
private:
    sf::Text text;
public:
    Button(float x, float y, float width, float height, const std::string& label, const std::string& action, const sf::Font& font);

    void draw(sf::RenderWindow& window);
    bool isClicked(sf::Vector2i mousePos);
    std::string getAction() { return action; }
    sf::RectangleShape shape;
    std::string action;
};

class TaskStyle {
private:
    sf::Text text;
    sf::RectangleShape shape;

public:
    TaskStyle(float y, float x, float width, const std::string& value, const sf::Font& font);
    void draw(sf::RenderWindow& window);
    void setPosition(float y);
};



class SFMLDisplay : public IDisplay {
private:
    sf::RenderWindow window;
    std::vector<Button> welcomeButtons;
    std::vector<Button> modeButtons;
    std::vector<Button> sortButtons;
    sf::Font font;
    std::vector<TaskStyle> taskRows;
    float scrollOffset; // Variable to control the scroll offset
    sf::Sprite backgroundSprite;
    sf::Texture backgroundTexture;
public:
    SFMLDisplay();
    void render(const std::vector<IModule*>& modules) override;
    void eventsManager(const std::vector<IModule*>& modules, IManager& manager);
    void TaskManagerStyle(const std::vector<IModule*>& modules, float yOffsetModules);
    void setScrollOffset(float offset); // Method to set the scroll offset
    void performanceStyle(sf::RenderWindow& window, const std::string& data, IModule* module, float& xOffset, float& yOffset, float moduleWidth, float moduleHeight, sf::Font& font);
};

// Affichage graphique avec SFML
