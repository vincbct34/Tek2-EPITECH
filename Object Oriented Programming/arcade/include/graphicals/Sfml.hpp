/*
** EPITECH PROJECT, 2025
** B-OOP-400-MPL-4-1-arcade-vincent.bichat
** File description:
** sfml
*/

#pragma once

#include "IGraphical.hpp"
#include "Entity.hpp"
#include "Event.hpp"

#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>
#include <SFML/System.hpp>
#include <SFML/Audio.hpp>

namespace Arcade {

    class SFML : public IGraphical {
        public:
            SFML() = default;
            ~SFML() = default;

            void init() override;
            void clearScreen() override;
            void drawElements(const std::vector<std::unique_ptr<IEntity>> &elements) override;
            void refresh() override;
            Event getInput(const std::vector<std::unique_ptr<IEntity>> &elements) override;
            void stop() override;
            std::pair<int, int> getWindowSize() const override;
            bool canUpdate() override;
            std::string getPlayerName() override;

        private:
            void drawTextElement(const AText* textElem, const sf::Vector2u& windowSize);
            void drawRectElement(const ARectangle* rectElem, const sf::Vector2u& windowSize);

            sf::RenderWindow _window;
            sf::Font _font;
            sf::Clock _clock;
    };

}
