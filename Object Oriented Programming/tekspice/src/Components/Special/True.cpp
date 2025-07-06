/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** True.cpp
*/

#include "True.hpp"

True::True()
{
}

True::~True()
{
}

void True::simulate(std::size_t tick)
{
    (void)tick;
}

nts::Tristate True::compute(std::size_t pin)
{
    (void)pin;
    return nts::True;
}

void True::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin)
{
    (void)pin;
    (void)other;
    (void)otherPin;
}
