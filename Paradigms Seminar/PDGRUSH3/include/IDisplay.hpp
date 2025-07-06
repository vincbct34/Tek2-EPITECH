/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#pragma once

#include "IModule.hpp"

#include <vector>

class IDisplay {
public:
    virtual ~IDisplay() = default;

    virtual void render(const std::vector<IModule*>& modules) = 0; // Affiche les modules
};

// Interface de base pour les affichages (textuel ou graphique)
