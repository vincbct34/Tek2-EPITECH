/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - The Fruits
*/

#pragma once

#include <string>

class IFruit {
    public:
        virtual ~IFruit() = default;

        virtual unsigned int getVitamins() const = 0;
        virtual std::string getName() const = 0;
        virtual bool isPeeled() const = 0;
        virtual void peel() = 0;
};

std::ostream &operator<<(std::ostream &s, IFruit const &fruit);
