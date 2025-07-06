/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - The Fruits
*/

#pragma once

#include "AFruit.hpp"

#include <string>

class ANut : public AFruit {
    public:
        ANut(std::string name, int vitamins);
        ~ANut();
};
