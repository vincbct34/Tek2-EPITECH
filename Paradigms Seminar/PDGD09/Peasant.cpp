/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - The Peasant
*/

#include "Peasant.hpp"

#include <iostream>

namespace {
    int clamp(int value, int min, int max) {
        if (value < min) return min;
        if (value > max) return max;

        return value;
    }
}

Peasant::Peasant(const std::string &name, int power)
    : name(name), powerPoints(clamp(power, 0, 100)), hpPoints(100)
{
    std::cout << name << " goes for an adventure." << std::endl;
}

Peasant::~Peasant()
{
    std::cout << name << " is back to his crops." << std::endl;
}

const std::string &Peasant::getName() const
{
    return name;
}

int Peasant::getPower() const
{
    return powerPoints;
}

int Peasant::getHp() const
{
    return hpPoints;
}

int Peasant::attack()
{
    if (this->getHp() == 0) {
        std::cout << name << " is out of combat." << std::endl;

        return 0;
    } else if (this->getPower() < 10) {
        std::cout << name << " is out of power." << std::endl;

        return 0;
    }

    this->powerPoints -= 10;
    std::cout << name << " tosses a stone." << std::endl;

    return 5;
}

int Peasant::special()
{
    if (this->getHp() == 0) {
        std::cout << name << " is out of combat." << std::endl;

        return 0;
    }

    std::cout << name << " doesn't know any special move." << std::endl;

    return 0;
}

void Peasant::rest()
{
    if (this->getHp() == 0) {
        std::cout << name << " is out of combat." << std::endl;

        return;
    }

    this->powerPoints = clamp(this->getPower() + 30, 0, 100);
    std::cout << name << " takes a nap." << std::endl;
}

void Peasant::damage(int damage)
{
    if (this->getHp() <= damage) {
        this->hpPoints = 0;
        std::cout << name << " is out of combat." << std::endl;

        return;
    }

    this->hpPoints = clamp(this->getHp() - damage, 0, 100);
    std::cout << name << " takes " << damage << " damage." << std::endl;
}

void Peasant::drink(const IPotion &potion)
{
    if (this->getHp() == 0) {
        std::cout << name << " is out of combat." << std::endl;

        return;
    }

    std::cout << name << " drinks a mysterious potion." << std::endl;

    if (potion.getType() == 0)
        this->hpPoints = clamp(this->getHp() + potion.getValue(), 0, 100);

    if (potion.getType() == 1)
        this->powerPoints = clamp(this->getPower() + potion.getValue(), 0, 100);
}

void Peasant::drink(const HealthPotion &potion)
{
    if (this->getHp() == 0) {
        std::cout << name << " is out of combat." << std::endl;

        return;
    }

    this->hpPoints = clamp(this->getHp() + potion.getValue(), 0, 100);
    std::cout << name << " feels rejuvenated." << std::endl;
}

void Peasant::drink(const PowerPotion &potion)
{
    if (this->getHp() == 0) {
        std::cout << name << " is out of combat." << std::endl;

        return;
    }

    this->powerPoints = clamp(this->getPower() + potion.getValue(), 0, 100);
    std::cout << name << "'s power is restored." << std::endl;
}

void Peasant::drink(const PoisonPotion &potion)
{
    if (this->getHp() == 0) {
        std::cout << name << " is out of combat." << std::endl;

        return;
    }

    this->hpPoints = clamp(this->getHp() + potion.getValue(), 0, 100);
    std::cout << name << " has been poisoned." << std::endl;
}
