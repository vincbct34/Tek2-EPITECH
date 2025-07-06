/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** Menu
*/

#include "Menu.hpp"

namespace Arcade {

    Menu::Menu() {
    }

    const std::vector<std::unique_ptr<IEntity>> &Menu::getElement() const {
        return _elements;
    }

    int Menu::updateGame(Event) {
        return 0;
    }

    std::string Menu::getChosenGame() const {
        return _chosenGame;
    }

    std::string Menu::getChosenGraphical() const {
        return _chosenGraphical;
    }

    std::string Menu::getGameName() const {
        return "lib/arcade_menu.so";
    }

    bool Menu::sendLibraryChangeSignal() {
        if (_isGameChosen && _isGraphicalChosen) {
            _gameState = GameState::STOPPED;
            return true;
        }
        return false;
    }
    
    extern "C" Menu *createGame() {
        return new Menu();
    }

}