/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - The Fruits
*/

#include "IFruit.hpp"

#include <iostream>

std::ostream &operator<<(std::ostream &s, IFruit const &fruit)
{
    auto peelVar = fruit.isPeeled() ? "true" : "false";

    s << "[name: ""\"" << fruit.getName() << "\", " << "vitamins: " << fruit.getVitamins() << ", " << "peeled: " << peelVar << "]"; 
    return s;
}
