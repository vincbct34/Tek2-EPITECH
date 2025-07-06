/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** Snake
*/

#include "Snake.hpp"
#include <string>

namespace Arcade {

    Snake::Snake() {
        _gameState = GameState::RUNNING;
    }

    void Snake::getInput(Event event) {
        if (_gameState == GameState::GAME_OVER && event.getType() == Event::TypeEvent::ENTER_KEY) {
            _gameState = GameState::STOPPED;
        }
    }

    const std::vector<std::unique_ptr<IEntity>> &Snake::getElement() const {
        return _elements;
    }

    std::string Snake::getChosenGraphical() const {
        return _chosenGraphical;
    }

    std::string Snake::getChosenGame() const {
        return _chosenGame;
    }

    std::string Snake::getGameName() const {
        return _chosenGame;
    }

    extern "C" Snake *createGame() {
        return new Snake();
    }
}
