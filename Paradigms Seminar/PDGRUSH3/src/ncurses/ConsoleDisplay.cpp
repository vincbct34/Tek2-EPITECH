/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#include "../../include/ConsoleDisplay.hpp"

#include <ncurses.h>
#include <unistd.h>
#include <sstream>

std::vector<std::string> splitStringToWords(const std::string& str) {
    std::vector<std::string> words;
    std::istringstream iss(str);
    std::string line;
    while (std::getline(iss, line)) {
        words.push_back(line);
    }
    return words;
}

void ConsoleDisplay::render(const std::vector<IModule*>& modules)
{
    initscr();
    start_color();
    init_pair(1, COLOR_BLACK, COLOR_WHITE); // Selected line color
    init_pair(2, COLOR_WHITE, COLOR_BLACK); // Default line color
    init_pair(3, COLOR_YELLOW, COLOR_BLACK); // Header color
    init_pair(4, COLOR_GREEN, COLOR_BLACK); // Title color
    noecho();
    curs_set(0);
    nodelay(stdscr, TRUE);

    int selectedIndex = 0;

    while (true) {
        clear();

        int line = 0;
        for (size_t i = 0; i < modules.size() - 2; ++i) {
            modules[i]->update();
            attron(COLOR_PAIR(3));
            mvprintw(line++, 2, "%s\n", modules[i]->getData().c_str());
        }
        attroff(COLOR_PAIR(3));
        attron(COLOR_PAIR(4));
        mvprintw(8, 2, "Gestionnaire de Processus");
        mvprintw(9, 2, "Filtrer : [ ] Search Process");
        attroff(COLOR_PAIR(4));
        mvprintw(11, 2, "%s", "PID      | NAME                 | CPU()   | MEM()       | NET       | DISK   |");
        mvprintw(12, 2, "------------------------------------------------------------------------------");

        if (!modules.empty()) {
            modules.back()->update();
            std::string lastModuleData = modules.back()->getData();
            std::vector<std::string> words = splitStringToWords(lastModuleData);

            for (size_t i = 0; i < words.size(); ++i) {
                if ((int)i == selectedIndex) {
                    attron(COLOR_PAIR(1));
                    std::vector<std::string> processInfo = splitStringToWords(words[i]);
                    
                    for (size_t j = 0; j < processInfo.size(); ++j) {
                        mvprintw(13 + i, 2 + j * 10, "%s", processInfo[j].c_str());
                    }
                    attroff(COLOR_PAIR(1));
                } else {
                    attron(COLOR_PAIR(2));
                    mvprintw(13 + i, 2, "%s", words[i].c_str());
                    attroff(COLOR_PAIR(2));
                }
            }
        }

        refresh();

        int ch = getch();
        if (ch == 'q' || ch == 'Q')
            break;
        else if (ch == 'z' || ch == 'Z') {
            if (selectedIndex > 0)
                selectedIndex--;
            else
                selectedIndex = 0;
        } else if (ch == 's' || ch == 'S') {
            if (selectedIndex < static_cast<long>(modules.back()->getData().size()) - 1)
                selectedIndex++;
        }

        usleep(50000); // Pause de 100ms pour éviter d'utiliser 100% du CPU
    }

    endwin();
}
// Implémentation de l'affichage textuel avec ncurses
