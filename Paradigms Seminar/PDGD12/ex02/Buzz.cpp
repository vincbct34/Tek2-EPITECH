/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - Simple inheritance
*/

#include "Buzz.hpp"

Buzz::Buzz(const std::string &name, const std::string &ascii)
    : Toy(BUZZ, name, ascii)
{
}

Buzz::~Buzz()
{
}
