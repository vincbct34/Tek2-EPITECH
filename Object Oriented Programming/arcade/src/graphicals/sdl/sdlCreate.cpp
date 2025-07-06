/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** sdlCreate
*/

#include "Sdl.hpp"

extern "C" Arcade::SDL *createGraphics() {
    return new Arcade::SDL();
}
