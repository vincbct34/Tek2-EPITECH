/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - The Fruits
*/

#include "IFruit.hpp"

#pragma once

class AFruit : public IFruit {
    public:
        AFruit(std::string name, int vitamins);
        ~AFruit();

        unsigned int getVitamins() const;
        std::string getName() const;
        bool isPeeled() const;
        void peel();

    protected:
        std::string _name;
        int _vitamins;
        bool _peeled;
};
