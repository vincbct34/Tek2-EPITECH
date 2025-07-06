/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - The Fruit Sorter
*/

#pragma once

#include "FruitBox.hpp"

class FruitUtils {
    public:
        FruitUtils();
        ~FruitUtils();

        static void sort(FruitBox &unsorted, FruitBox &lemons, FruitBox &citrus, FruitBox &berry);
        static FruitBox **pack(IFruit **fruits, unsigned int boxSize);
        static IFruit **unpack(FruitBox **fruitBoxes);
};
