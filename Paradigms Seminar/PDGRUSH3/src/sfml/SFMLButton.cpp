/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#include "../../include/SFMLDisplay.hpp"
#include <cmath>

Button::Button(float x, float y, float width, float height, const std::string& label, const std::string& action, const sf::Font& font)
{
    shape.setSize({width, height});
    shape.setPosition(x, y);
    shape.setFillColor(sf::Color(255, 255, 255, 0));
    shape.setOutlineThickness(2);
    shape.setOutlineColor(sf::Color::White);

    text.setFont(font);
    text.setString(label);
    text.setFillColor(sf::Color::White);
    text.setCharacterSize(20);

    auto center = text.getGlobalBounds().getSize() / 2.f;
    auto localBounds = center + text.getGlobalBounds().getPosition();

    auto rounded = sf::Vector2f(std::round(localBounds.x), std::round(localBounds.y));

    text.setOrigin(rounded);
    text.setPosition(x + width / 2, y + height / 2);
    this->action = action;
}


void Button::draw(sf::RenderWindow& window)
{
    window.draw(shape);
    window.draw(text);
}

bool Button::isClicked(sf::Vector2i mousePos)
{
    return shape.getGlobalBounds().contains(static_cast<sf::Vector2f>(mousePos));
}
