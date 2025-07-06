/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#include "../../include/SFMLDisplay.hpp"
#include <cmath>
#include <iostream>

TaskStyle::TaskStyle(float y, float x, float width, const std::string& value, const sf::Font& font) {
        shape.setSize({width, 40});
        shape.setPosition(x, y);
        shape.setFillColor(sf::Color(50, 50, 50, 200));  // Fond gris semi-transparent

        text.setFont(font);
        text.setString(value);
        text.setCharacterSize(20);
        text.setFillColor(sf::Color::White);

        // Centrer le texte horizontalement
        sf::FloatRect textBounds = text.getLocalBounds();
        text.setPosition(
            x + (width / 2.f) - (textBounds.width / 2.f),
            y + (40 / 2.f) - (textBounds.height / 2.f)
        );
    }

void TaskStyle::draw(sf::RenderWindow& window) {
    window.draw(shape);
    window.draw(text);
}

void TaskStyle::setPosition(float y) {
    shape.setPosition(shape.getPosition().x, y);
    text.setPosition(text.getPosition().x, y + 10);
}
