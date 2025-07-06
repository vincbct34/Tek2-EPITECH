/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 6 - The Potions
*/

#include "PowerPotion.hpp"

PowerPotion::PowerPotion()
{
    this->type = 1;
    this->value = 50;
}

PowerPotion::~PowerPotion()
{
}

int PowerPotion::getType() const
{
    return this->type;
}

int PowerPotion::getValue() const
{
    return this->value;
}
