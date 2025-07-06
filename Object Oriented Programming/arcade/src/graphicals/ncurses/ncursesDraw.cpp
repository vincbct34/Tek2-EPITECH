/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** ncursesDraw
*/

#include "Ncurses.hpp"

namespace Arcade {

    void Ncurse::drawTextElement(const AText *textElem, int maxX, int maxY) {
        int posX = (textElem->getPos().first * maxX) / 100;
        int posY = (textElem->getPos().second * maxY) / 100;

        posX = std::max(0, std::min(posX, maxX - 1));
        posY = std::max(0, std::min(posY, maxY - 1));

        auto [r, g, b] = textElem->getColor();
        int colorPairId = 10 + (r + g + b) % 240;
        init_color(colorPairId, r * 4, g * 4, b * 4);
        init_pair(colorPairId, colorPairId, COLOR_BLACK);

        wattron(_win, COLOR_PAIR(colorPairId));
        mvwprintw(_win, posY, posX, "%s", textElem->getText().c_str());
        wattroff(_win, COLOR_PAIR(colorPairId));

        if (textElem->isHighlighted()) {
            wattron(_win, A_BOLD);
            mvwprintw(_win, posY, posX, "%s", textElem->getText().c_str());
            wattroff(_win, A_BOLD);
        }
    }

    void Ncurse::drawRectElement(const ARectangle *rectElem, int maxX, int maxY) {
        int posX = (rectElem->getPos().first * maxX) / 100;
        int posY = (rectElem->getPos().second * maxY) / 100;
        mvwprintw(_win, posY, posX, "#");
    }

}
