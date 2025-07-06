/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 6 - The Potions
*/

#pragma once

#include "IPotion.hpp"

class PowerPotion : public IPotion {
    public:
        PowerPotion();
        ~PowerPotion();

        int getType() const override;
        int getValue() const override;
};
