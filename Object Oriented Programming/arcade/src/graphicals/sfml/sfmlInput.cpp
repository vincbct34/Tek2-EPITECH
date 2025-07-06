/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** sfmlInput
*/

#include "Sfml.hpp"

namespace Arcade {

    Event SFML::getInput(const std::vector<std::unique_ptr<IEntity>> &elements) {
        Event event;
        sf::Event sfEvent;

        while (_window.pollEvent(sfEvent)) {
            if (sfEvent.type == sf::Event::Closed) {
                _window.close();
            } else if (sfEvent.type == sf::Event::KeyPressed) {
                switch (sfEvent.key.code) {
                    case sf::Keyboard::Up: event.setType(Event::TypeEvent::UP_ARROW); break;
                    case sf::Keyboard::Down: event.setType(Event::TypeEvent::DOWN_ARROW); break;
                    case sf::Keyboard::Left: event.setType(Event::TypeEvent::LEFT_ARROW); break;
                    case sf::Keyboard::Right: event.setType(Event::TypeEvent::RIGHT_ARROW); break;
                    case sf::Keyboard::Enter: event.setType(Event::TypeEvent::ENTER_KEY); break;
                    case sf::Keyboard::Escape: event.setType(Event::TypeEvent::ESCAPE_KEY); break;
                    case sf::Keyboard::Space: event.setType(Event::TypeEvent::SPACE_KEY); break;
                    case sf::Keyboard::Tab: event.setType(Event::TypeEvent::TAB_KEY); break;
                    case sf::Keyboard::LShift: event.setType(Event::TypeEvent::SHIFT_KEY); break;
                    case sf::Keyboard::BackSpace: event.setType(Event::TypeEvent::BACKSPACE); break;
                    default: event.setType(Event::TypeEvent::UNDEFINED); break;
                }
            } else if (sfEvent.type == sf::Event::MouseButtonPressed && sfEvent.mouseButton.button == sf::Mouse::Left) {
                event.setType(Event::TypeEvent::LMOUSE);
                for (const auto &elem : elements) {
                    auto textElem = dynamic_cast<AText*>(elem.get());
                    if (textElem) {
                        sf::Vector2i mousePos = sf::Mouse::getPosition(_window);
                        sf::Vector2u winSize = _window.getSize();

                        float posX = (textElem->getPos().first * winSize.x) / 100;
                        float posY = (textElem->getPos().second * winSize.y) / 100;
                        float width = textElem->getSize().first;
                        float height = textElem->getSize().second;

                        if (mousePos.x >= posX && mousePos.x <= posX + width &&
                            mousePos.y >= posY && mousePos.y <= posY + height) {
                            event.setClickedEntity(textElem);
                            break;
                        }
                    }
                }
            }
        }
        return event;
    }

}
