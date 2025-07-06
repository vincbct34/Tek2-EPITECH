/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 5 - Nesting
*/

#pragma once

#include "Toy.hpp"

class Buzz : public Toy {
    public:
        Buzz(const std::string &name, const std::string &ascii = "buzz.txt");
        ~Buzz();

        void speak(const std::string &statement) override;

        bool speak_es(const std::string &statement) override;
};
