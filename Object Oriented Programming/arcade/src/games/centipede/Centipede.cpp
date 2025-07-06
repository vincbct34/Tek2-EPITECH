/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** snake
*/

#include "Centipede.hpp"

namespace Arcade {

    Centipede::Centipede() {
        _gameState = GameState::RUNNING;
    }

    const std::vector<std::unique_ptr<IEntity>> &Centipede::getElement() const {
        return _elements;
    }

    void Centipede::getInput(Event event) {
        if (_gameState == GameState::GAME_OVER && event.getType() == Event::TypeEvent::ENTER_KEY) {
            _gameState = GameState::STOPPED;
        }
    }

    std::string Centipede::getGameName() const {
        return _chosenGame;
    }

    std::string Centipede::getChosenGame() const {
        return _chosenGame;
    }

    std::string Centipede::getChosenGraphical() const {
        return _chosenGraphical;
    }

    extern "C" Centipede *createGame() {
        return new Centipede();
    }

}
