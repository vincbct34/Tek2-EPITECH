/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 5 - Nesting
*/

#include "Buzz.hpp"

#include <iostream>

Buzz::Buzz(const std::string &name, const std::string &ascii)
    : Toy(BUZZ, name, ascii)
{
}

Buzz::~Buzz()
{
}

void Buzz::speak(const std::string &statement)
{
    std::cout << "BUZZ: " << this->getName() << " \"" << statement << "\"" << std::endl;
}

bool Buzz::speak_es(const std::string &statement)
{
    std::cout << "BUZZ: " << this->getName() << " senorita \"" << statement << "\" senorita" << std::endl;
    return true;
}
