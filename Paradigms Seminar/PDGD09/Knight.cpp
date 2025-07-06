/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1 - The Knight
*/

#include "Knight.hpp"

#include <iostream>

namespace {
    int clamp(int value, int min, int max) {
        if (value < min) return min;
        if (value > max) return max;

        return value;
    }
}

Knight::Knight(const std::string &name, int power)
    : Peasant(name, clamp(power, 0, 100))
{
    std::cout << name << " vows to protect the kingdom." << std::endl;
}

Knight::~Knight()
{
    std::cout << name << " takes off his armor." << std::endl;
}

int Knight::attack()
{
    if (this->getHp() == 0) {
        std::cout << name << " is out of combat." << std::endl;

        return 0;
    } else if (this->getPower() < 10) {
        std::cout << name << " is out of power." << std::endl;

        return 0;
    }

    powerPoints -= 10;
    std::cout << name << " strikes with his sword." << std::endl;

    return 20;
}

int Knight::special()
{
    if (this->getHp() == 0) {
        std::cout << name << " is out of combat." << std::endl;

        return 0;
    } else if (this->getPower() < 30) {
        std::cout << name << " is out of power." << std::endl;

        return 0;
    }

    powerPoints -= 30;
    std::cout << name << " impales his enemy." << std::endl;

    return 50;
}

void Knight::rest()
{
    if (this->getHp() == 0) {
        std::cout << name << " is out of combat." << std::endl;

        return;
    }

    this->powerPoints = clamp(this->getPower() + 50, 0, 100);
    std::cout << name << " eats." << std::endl;
}
