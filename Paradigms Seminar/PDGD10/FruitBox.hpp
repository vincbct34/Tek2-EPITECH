/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1 - The Fruit Box
*/

#pragma once

#include "IFruit.hpp"

#include <iostream>
#include <vector>

class FruitBox {
    public:
        FruitBox(unsigned int size);
        ~FruitBox();

        unsigned int getSize() const;
        unsigned int nbFruits() const;
        bool pushFruit(IFruit *fruit);
        IFruit *popFruit();

        IFruit *getFruitAt(unsigned int idx) const;

    private:
        unsigned int _size;
        std::vector<IFruit *> _fruits;
};

std::ostream &operator<<(std::ostream &s, FruitBox const &box);
