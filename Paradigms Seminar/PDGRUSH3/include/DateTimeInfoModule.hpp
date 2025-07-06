/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Date and Time Module
*/

#pragma once

#include "IModule.hpp"
#include <string>
#include <SFML/Graphics.hpp>

class DateTimeModule : public IModule {
public:
    DateTimeModule();

    void update() override;
    
    const std::string& getDateTime() const { return dateTimeStr; }
    std::string getData() const override;

private:
    std::string _name = "DateTimeModule";
    std::string dateTimeStr;
};
