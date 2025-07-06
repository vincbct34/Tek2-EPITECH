/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#include "../../include/IManager.hpp"

#include <algorithm>

void IManager::addModule(IModule* module)
{
    modules.push_back(module);
}

void IManager::removeModule(IModule* module)
{
    modules.erase(std::remove(modules.begin(), modules.end(), module), modules.end());
}

void IManager::updateModules()
{
    for (auto& module : modules)
        module->update();
}

const std::vector<IModule*>& IManager::getModules() const
{
    return modules;
}

void IManager::setMode(IModule::Mode mode)
{
    this->mode = mode;
}

IModule::Mode IManager::getMode() const
{
    return mode;
}

// Implémentation du gestionnaire de modules
