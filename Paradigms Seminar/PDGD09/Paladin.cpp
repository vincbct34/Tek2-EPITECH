/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 4 - The Paladin
*/

#include "Paladin.hpp"
#include "Enchanter.hpp"
#include "Priest.hpp"

#include <iostream>

Paladin::Paladin(const std::string &name, int power)
    : Peasant(name, power), Knight(name, power), Enchanter(name, power), Priest(name, power)
{
    std::cout << name << " fights for the light." << std::endl;
}

Paladin::~Paladin()
{
    std::cout << name << " is blessed." << std::endl;
}

int Paladin::attack()
{
    return Knight::attack();
}

int Paladin::special()
{
    return Enchanter::special();
}

void Paladin::rest()
{
    Priest::rest();
}
