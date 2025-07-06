/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 3 - The Priest
*/

#pragma once

#include "Enchanter.hpp"

class Priest : virtual public Enchanter {
    public:
        Priest(const std::string &name, int power);
        ~Priest();

        void rest() override;
};
