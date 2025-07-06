/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - Simple inheritance
*/

#include "Woody.hpp"

Woody::Woody(const std::string &name, const std::string &ascii)
    : Toy(WOODY, name, ascii)
{
}

Woody::~Woody()
{
}
