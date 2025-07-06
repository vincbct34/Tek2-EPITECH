/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Rush 3
*/

#pragma once

#include <SFML/Graphics.hpp>
#include <SFML/Graphics/Font.hpp>

class IModule {
public:
    enum class Mode {
        Performance,
        TaskManager,
        WelcomePage
    };

    enum class DataType {
        Text,
        Graphical
    };

    Mode mode;
    DataType dataType;
    std::string name;

    IModule(Mode mode, DataType dataType, std::string name) : mode(mode), dataType(dataType), name(name) {}
    ~IModule() = default;

    virtual void update() = 0;
    virtual std::string getData() const = 0;
    virtual std::string getName() const { return name; }
};

// Interface de base pour tous les modules de monitoring
