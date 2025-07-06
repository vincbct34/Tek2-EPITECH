/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - The Fruits
*/

#include "AFruit.hpp"

AFruit::AFruit(std::string name, int vitamins)
    : _name(name), _vitamins(vitamins), _peeled(false)
{
}

AFruit::~AFruit()
{
}

unsigned int AFruit::getVitamins() const
{
    return isPeeled() ? _vitamins : 0;
}

std::string AFruit::getName() const
{
    return _name;
}

bool AFruit::isPeeled() const
{
    return _peeled;
}

void AFruit::peel()
{
    _peeled = true;
}
