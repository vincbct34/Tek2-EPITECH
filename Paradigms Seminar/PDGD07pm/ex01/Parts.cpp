/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1 - KoalaBot
*/

#include "Parts.hpp"

#include <iostream>

Arms::Arms(std::string serial, bool functional)
    : _serial(serial), _functional(functional)
{
}

Arms::~Arms()
{
}

Legs::Legs(std::string serial, bool functional)
    : _serial(serial), _functional(functional)
{
}

Legs::~Legs()
{
}

Head::Head(std::string serial, bool functional)
    : _serial(serial), _functional(functional)
{
}

Head::~Head()
{
}

void Arms::informations() const
{
    std::cout << "[Parts] Arms " << _serial << " status : " << (_functional ? "OK" : "KO") << std::endl;
}

void Legs::informations() const
{
    std::cout << "[Parts] Legs " << _serial << " status : " << (_functional ? "OK" : "KO") << std::endl;
}

void Head::informations() const
{
    std::cout << "[Parts] Head " << _serial << " status : " << (_functional ? "OK" : "KO") << std::endl;
}
