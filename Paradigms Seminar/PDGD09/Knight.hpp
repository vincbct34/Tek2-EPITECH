/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1 - The Knight
*/

#pragma once

#include "Peasant.hpp"

class Knight : virtual public Peasant {
    public:
        Knight(const std::string &name, int power);
        ~Knight();

        int attack() override;
        int special() override;
        void rest() override;
};
