/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** Sfml
*/

#include "Sfml.hpp"

namespace Arcade {
    
    void SFML::init() {
        _window.create(sf::VideoMode(800, 600), "Arcade");
        _window.setFramerateLimit(60);
        _font.loadFromFile("src/graphicals/fonts/LEMONMILK-Medium.otf");
        _clock.restart();
    }

    void SFML::clearScreen() {
        _window.clear(sf::Color::Black);
    }

    void SFML::drawElements(const std::vector<std::unique_ptr<IEntity>> &elements) {
        sf::Vector2u windowSize = _window.getSize();

        for (const auto &elem : elements) {
            if (auto textElem = dynamic_cast<AText*>(elem.get()))
                drawTextElement(textElem, windowSize);
            else if (auto rectElem = dynamic_cast<ARectangle*>(elem.get()))
                drawRectElement(rectElem, windowSize);
        }
    }

    void SFML::refresh() {
        _window.display();
    }

    void SFML::stop() {
        _window.close();
    }

    std::pair<int, int> SFML::getWindowSize() const {
        return { static_cast<int>(_window.getSize().x), static_cast<int>(_window.getSize().y) };
    }

    bool SFML::canUpdate() {
        if (_clock.getElapsedTime().asMilliseconds() >= 2) {
            _clock.restart();
            return true;
        }
        return false;
    }

    std::string Arcade::SFML::getPlayerName()
    {
        std::string name;
        sf::Text prompt;
        sf::Text input;
        sf::Event event;

        prompt.setFont(_font);
        prompt.setString("Enter your name:");
        prompt.setCharacterSize(36);
        prompt.setFillColor(sf::Color::White);
        prompt.setPosition(200, 200);

        input.setFont(_font);
        input.setCharacterSize(30);
        input.setFillColor(sf::Color::Green);
        input.setPosition(200, 260);

        while (_window.isOpen()) {
            while (_window.pollEvent(event)) {
                if (event.type == sf::Event::Closed) {
                    _window.close();
                    return "";
                } else if (event.type == sf::Event::TextEntered) {
                    if (event.text.unicode == '\b' && !name.empty()) {
                        name.pop_back();
                    } else if (event.text.unicode < 128 && std::isprint(event.text.unicode)) {
                        name += static_cast<char>(event.text.unicode);
                    }
                    input.setString(name);
                } else if (event.type == sf::Event::KeyPressed && event.key.code == sf::Keyboard::Enter) {
                    return name;
                }
            }

            _window.clear();
            _window.draw(prompt);
            _window.draw(input);
            _window.display();
        }
        return "";
    }

}
