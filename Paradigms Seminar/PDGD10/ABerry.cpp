/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - The Fruits
*/

#include "ABerry.hpp"

ABerry::ABerry(std::string name, int vitamins)
    : AFruit(name, vitamins)
{
    _peeled = true;
}

ABerry::~ABerry()
{
}
