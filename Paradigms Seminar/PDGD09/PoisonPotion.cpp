/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 6 - The Potions
*/

#include "PoisonPotion.hpp"

PoisonPotion::PoisonPotion()
{
    this->type = 0;
    this->value = -50;
}

PoisonPotion::~PoisonPotion()
{
}

int PoisonPotion::getType() const
{
    return this->type;
}

int PoisonPotion::getValue() const
{
    return this->value;
}
