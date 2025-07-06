/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** ncursesCreate
*/

#include "Ncurses.hpp"

extern "C" Arcade::Ncurse *createGraphics() {
    return new Arcade::Ncurse();
}
