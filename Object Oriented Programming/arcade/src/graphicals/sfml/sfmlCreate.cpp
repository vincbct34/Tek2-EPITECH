/*
** EPITECH PROJECT, 2025
** Arcade
** File description:
** sfmlCreate
*/

#include "Sfml.hpp"

extern "C" Arcade::SFML *createGraphics() {
    return new Arcade::SFML();
}
