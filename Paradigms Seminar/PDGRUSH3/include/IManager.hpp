/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#pragma once

#include "IModule.hpp"

#include <vector>

class IManager {
public:
    IModule::Mode mode = IModule::Mode::WelcomePage;

    void addModule(IModule* module);
    void removeModule(IModule* module);
    void updateModules();
    const std::vector<IModule*>& getModules() const;
    void setMode(IModule::Mode mode);
    IModule::Mode getMode() const;

private:
    std::vector<IModule*> modules;
};

// Gestionnaire de modules permettant d'ajouter, supprimer et mettre à jour les modules
