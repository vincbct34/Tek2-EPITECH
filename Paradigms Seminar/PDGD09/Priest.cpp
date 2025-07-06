/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 3 - The Priest
*/

#include "Priest.hpp"

#include <iostream>

namespace {
    int clamp(int value, int min, int max) {
        if (value < min) return min;
        if (value > max) return max;

        return value;
    }
}

Priest::Priest(const std::string &name, int power)
    : Peasant(name, clamp(power, 0, 100)), Enchanter(name, clamp(power, 0, 100))
{
    std::cout << name << " enters in the order." << std::endl;
}

Priest::~Priest()
{
    std::cout << name << " finds peace." << std::endl;
}

void Priest::rest()
{
    if (this->getHp() == 0) {
        std::cout << name << " is out of combat." << std::endl;

        return;
    }

    this->hpPoints = clamp(this->getHp() + 100, 0, 100);
    this->powerPoints = clamp(this->getPower() + 100, 0, 100);
    std::cout << name << " prays." << std::endl;
}
