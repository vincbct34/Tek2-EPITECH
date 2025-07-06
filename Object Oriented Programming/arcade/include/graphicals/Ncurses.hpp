/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** Ncurses
*/

#pragma once

#include "IGraphical.hpp"
#include "Entity.hpp"
#include "Event.hpp"

#include <ncurses.h>
#include <string>
#include <unistd.h>

namespace Arcade {

    class Ncurse : public IGraphical {
        public:
            Ncurse() = default;
            ~Ncurse() = default;

            void init() override;
            void clearScreen() override;
            void drawElements(const std::vector<std::unique_ptr<IEntity>> &elements) override;
            void refresh() override;
            Event getInput(const std::vector<std::unique_ptr<IEntity>> &elements) override;
            void stop() override;
            std::pair<int, int> getWindowSize() const override;
            bool canUpdate() override { return true; }
            std::string getPlayerName() override;

        private:
            WINDOW *_win = nullptr;
            void drawTextElement(const AText *textElem, int maxX, int maxY);
            void drawRectElement(const ARectangle *rectElem, int maxX, int maxY);
    };

}
