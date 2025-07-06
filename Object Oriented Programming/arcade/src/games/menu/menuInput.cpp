/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** menuInput
*/

#include "Menu.hpp"

namespace Arcade {

    void Menu::getInput(Event event) {
        if (event.getType() == Event::TypeEvent::ENTER_KEY) {
            handleEnter();
        } else if (event.getType() == Event::TypeEvent::UP_ARROW) {
            navigateOptions(-1);
        } else if (event.getType() == Event::TypeEvent::DOWN_ARROW) {
            navigateOptions(1);
        }

        displayScores();
    }

    void Menu::handleEnter() {
        int idToCheck = _isGameChosen ? 2 : 1;
        for (const auto &elem : _elements) {
            auto textElem = dynamic_cast<AText*>(elem.get());
            if (textElem && textElem->isHighlighted() && textElem->getID() == idToCheck) {
                if (!_isGameChosen) {
                    _chosenGame = textElem->getText();
                    _isGameChosen = true;
                } else if (!_isGraphicalChosen) {
                    _chosenGraphical = textElem->getText();
                    _isGraphicalChosen = true;
                }
                break;
            }
        }
    }

    void Menu::navigateOptions(int direction) {
        int idToCheck = _isGameChosen ? 2 : 1;
        for (size_t i = 0; i < _elements.size(); ++i) {
            auto textElem = dynamic_cast<AText*>(_elements[i].get());
            if (textElem && textElem->getID() == idToCheck && textElem->isHighlighted()) {
                int nextIndex = static_cast<int>(i) + direction;
                if (nextIndex >= 0 && nextIndex < static_cast<int>(_elements.size())) {
                    auto nextElem = dynamic_cast<AText*>(_elements[nextIndex].get());
                    if (nextElem && nextElem->getID() == idToCheck) {
                        textElem->setHighlighted(false);
                        nextElem->setHighlighted(true);
                    }
                }
                break;
            }
        }
    }

}