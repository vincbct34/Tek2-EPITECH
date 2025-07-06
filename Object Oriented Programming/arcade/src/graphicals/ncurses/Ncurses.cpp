/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** Ncurses
*/

#include "Ncurses.hpp"

namespace Arcade {
    
    void Ncurse::init() {
        initscr();
        _win = newwin(0, 0, 0, 0);
        cbreak();
        noecho();
        keypad(_win, TRUE);
        curs_set(0);
        nodelay(_win, TRUE);
        start_color();
        for (int i = 1; i <= 7; ++i)
            init_pair(i, i, COLOR_BLACK);
    }

    void Ncurse::clearScreen() {
        usleep(50000);
        wclear(_win);
    }

    void Ncurse::drawElements(const std::vector<std::unique_ptr<IEntity>> &elements) {
        int maxY, maxX;
        getmaxyx(_win, maxY, maxX);

        for (const auto &elem : elements) {
            if (auto text = dynamic_cast<AText*>(elem.get()))
                drawTextElement(text, maxX, maxY);
            else if (auto rect = dynamic_cast<ARectangle*>(elem.get()))
                drawRectElement(rect, maxX, maxY);
        }
    }

    void Ncurse::refresh() {
        wrefresh(_win);
    }

    void Ncurse::stop() {
        delwin(_win);
        endwin();
    }

    std::pair<int, int> Ncurse::getWindowSize() const {
        return {getmaxx(_win), getmaxy(_win)};
    }

    std::string Arcade::Ncurse::getPlayerName()
    {
        std::string name;
        int ch;

        wclear(_win);
        mvwprintw(_win, 5, 10, "Enter your name (press Enter to confirm): ");
        wrefresh(_win);

        echo();       // Affiche les caractères tapés
        curs_set(1);  // Affiche le curseur

        while ((ch = wgetch(_win)) != '\n') {
            if ((ch == KEY_BACKSPACE || ch == 127) && !name.empty()) {
                name.pop_back();
                mvwprintw(_win, 6, 10, "%s ", name.c_str());
                wmove(_win, 6, 10 + name.size());
            } else if (isprint(ch)) {
                name += static_cast<char>(ch);
                mvwprintw(_win, 6, 10, "%s", name.c_str());
                wmove(_win, 6, 10 + name.size());
            }
            wrefresh(_win);
        }

        noecho();
        curs_set(0);
        return name;
    }

}
