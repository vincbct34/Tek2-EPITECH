/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Storage Module
*/

#pragma once

#include "IModule.hpp"
#include <string>
#include <SFML/Graphics.hpp>

class StorageModule : public IModule {
public:
    StorageModule();

    void update() override;
    
    long getTotalStorage() const { return totalStorage; }
    long getUsedStorage() const { return usedStorage; }
    long getFreeStorage() const { return freeStorage; }
    std::string getData() const override;

private:
    void updateStorageInfo();
    std::string formatSize(long bytes) const;

    std::string _name = "StorageModule";
    long totalStorage;
    long usedStorage;
    long freeStorage;
};

