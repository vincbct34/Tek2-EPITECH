/*
** EPITECH PROJECT, 2025
** B-OOP-400-MPL-4-1-arcade-vincent.bichat
** File description:
** IGame
*/

#pragma once

#include "Entity.hpp"
#include "Event.hpp"

#include <unordered_map>
#include <vector>

namespace Arcade {
    /**
     * @class IGame
     * @brief Interface for the Arcade games.
     * 
     * This interface defines the methods that must be implemented by any game.
     */
    class IGame {
        public:
            virtual ~IGame() = default;

            enum class GameState {
                STOPPED,
                RUNNING,
                GAME_OVER,
            };

            virtual void init(std::vector<std::string>, std::vector<std::string>, int, int, std::unordered_map<std::string, std::array<std::pair<std::string, int>, 3>>) = 0;
            virtual void getInput(Event event) = 0;
            virtual void displayEndScreen() = 0;

            virtual const std::vector<std::unique_ptr<IEntity>> &getElement() const = 0;

            virtual int updateGame(Event event) = 0;
            
            virtual std::string getChosenGraphical() const = 0;
            virtual std::string getChosenGame() const = 0;
            virtual std::string getGameName() const = 0;
            
            GameState getGameState() const {
                return _gameState;
            }

            virtual bool sendLibraryChangeSignal() {
                return false;
            }
        
        protected:
            GameState _gameState;
    };
}
