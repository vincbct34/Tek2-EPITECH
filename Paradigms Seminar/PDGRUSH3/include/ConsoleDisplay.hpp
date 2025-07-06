/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#pragma once

#include "IDisplay.hpp"

#include <ncurses.h>
#include <vector>

class ConsoleDisplay : public IDisplay {
public:
    void render(const std::vector<IModule*>& modules) override;
};

// Affichage textuel avec ncurses
