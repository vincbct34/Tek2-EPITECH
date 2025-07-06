/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** sfmlDraw
*/

#include "Sfml.hpp"

namespace Arcade {

    void SFML::drawTextElement(const AText* textElem, const sf::Vector2u& windowSize) {
        sf::Text text;
        text.setFont(_font);
        text.setString(textElem->getText());
        text.setCharacterSize(24);

        float posX = (textElem->getPos().first * windowSize.x) / 100;
        float posY = (textElem->getPos().second * windowSize.y) / 100;
        text.setPosition(posX, posY);

        auto [r, g, b] = textElem->getColor();
        text.setFillColor(sf::Color(r, g, b));

        if (textElem->isHighlighted())
            text.setStyle(sf::Text::Bold | sf::Text::Underlined);

        sf::FloatRect bounds = text.getLocalBounds();
        text.setOrigin(bounds.width / 2, bounds.height / 2);
        _window.draw(text);
    }

    void SFML::drawRectElement(const ARectangle* rectElem, const sf::Vector2u& windowSize) {
        sf::RectangleShape rectangle;

        float posX = (rectElem->getPos().first * windowSize.x) / 100;
        float posY = (rectElem->getPos().second * windowSize.y) / 100;
        float sizeX = (rectElem->getSize().first * windowSize.x) / 100;
        float sizeY = (rectElem->getSize().second * windowSize.y) / 100;

        rectangle.setSize(sf::Vector2f(sizeX, sizeY));
        rectangle.setPosition(posX, posY);

        auto [r, g, b] = rectElem->getColor();
        rectangle.setFillColor(sf::Color(r, g, b));

        _window.draw(rectangle);
    }

}
