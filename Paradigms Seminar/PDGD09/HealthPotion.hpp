/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 6 - The Potions
*/

#pragma once

#include "IPotion.hpp"

class HealthPotion : public IPotion {
    public:
        HealthPotion();
        ~HealthPotion();

        int getType() const override;
        int getValue() const override;
};
