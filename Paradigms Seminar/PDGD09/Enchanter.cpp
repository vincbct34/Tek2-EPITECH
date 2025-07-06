/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - The Enchanter
*/

#include "Enchanter.hpp"

#include <iostream>

namespace {
    int clamp(int value, int min, int max) {
        if (value < min) return min;
        if (value > max) return max;

        return value;
    }
}

Enchanter::Enchanter(const std::string &name, int power)
    : Peasant(name, clamp(power, 0, 100))
{
    std::cout << name << " learns magic from his spellbook." << std::endl;
}

Enchanter::~Enchanter()
{
    std::cout << name << " closes his spellbook." << std::endl;
}

int Enchanter::attack()
{
    if (this->getHp() == 0) {
        std::cout << name << " is out of combat." << std::endl;

        return 0;
    }

    std::cout << name << " doesn't know how to fight." << std::endl;

    return 0;
}

int Enchanter::special()
{
    if (this->getHp() == 0) {
        std::cout << name << " is out of combat." << std::endl;

        return 0;
    } else if (this->getPower() < 50) {
        std::cout << name << " is out of power." << std::endl;

        return 0;
    }

    powerPoints -= 50;
    std::cout << name << " casts a fireball." << std::endl;

    return 99;
}

void Enchanter::rest()
{
    if (this->getHp() == 0) {
        std::cout << name << " is out of combat." << std::endl;

        return;
    }

    this->powerPoints = clamp(this->getPower() + 100, 0, 100);
    std::cout << name << " meditates." << std::endl;
}
