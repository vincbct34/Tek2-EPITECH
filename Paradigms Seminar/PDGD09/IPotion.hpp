/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 6 - The Potions
*/

#pragma once

class IPotion {
    public:
        IPotion() = default;
        ~IPotion() = default;

        virtual int getType() const = 0;
        virtual int getValue() const = 0;

    protected:
        int type = -1;
        int value;
};
