/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 5 - Nesting
*/

#include "Woody.hpp"

#include <iostream>

Woody::Woody(const std::string &name, const std::string &ascii)
    : Toy(WOODY, name, ascii)
{
}

Woody::~Woody()
{
}

void Woody::speak(const std::string &statement)
{
    std::cout << "WOODY: " << this->getName() << " \"" << statement << "\"" << std::endl;
}
