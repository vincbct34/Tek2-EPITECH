/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - The Enchanter
*/

#pragma once

#include "Peasant.hpp"

class Enchanter : virtual public Peasant {
    public:
        Enchanter(const std::string &name, int power);
        ~Enchanter();

        int attack() override;
        int special() override;
        void rest() override;
};
