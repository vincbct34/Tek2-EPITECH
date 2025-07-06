/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** False.cpp
*/

#include "False.hpp"

False::False()
{
}

False::~False()
{
}

void False::simulate(std::size_t tick)
{
    (void)tick;
}

nts::Tristate False::compute(std::size_t pin)
{
    (void)pin;
    return nts::False;
}

void False::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin)
{
    (void)pin;
    (void)other;
    (void)otherPin;
}
