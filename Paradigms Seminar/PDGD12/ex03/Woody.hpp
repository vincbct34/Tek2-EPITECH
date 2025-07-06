/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 3 - Ponymorphism
*/

#pragma once

#include "Toy.hpp"

class Woody : public Toy {
    public:
        Woody(const std::string &name, const std::string &ascii = "woody.txt");
        ~Woody();

        void speak(const std::string &statement) override;
};
