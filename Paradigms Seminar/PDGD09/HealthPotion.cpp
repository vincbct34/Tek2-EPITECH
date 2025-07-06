/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 6 - The Potions
*/

#include "HealthPotion.hpp"

HealthPotion::HealthPotion()
{
    this->type = 0;
    this->value = 50;
}

HealthPotion::~HealthPotion()
{
}

int HealthPotion::getType() const
{
    return this->type;
}

int HealthPotion::getValue() const
{
    return this->value;
}
