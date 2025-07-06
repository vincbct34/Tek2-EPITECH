/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - Simple inheritance
*/

#pragma once

#include "Toy.hpp"

class Woody : public Toy {
    public:
        Woody(const std::string &name, const std::string &ascii = "woody.txt");
        ~Woody();
};
