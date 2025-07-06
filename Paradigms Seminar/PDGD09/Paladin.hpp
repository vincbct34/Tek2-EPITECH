/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 4 - The Paladin
*/

#pragma once

#include "Knight.hpp"
#include "Priest.hpp"

#include <string>

class Paladin : virtual public Knight, virtual public Priest {
    public:
        Paladin(const std::string &name, int power);
        ~Paladin();

        int attack() override;
        int special() override;
        void rest() override;
};
