/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - The Fruit Sorter
*/

#include "FruitUtils.hpp"
#include "IFruit.hpp"
#include "Lemon.hpp"
#include "ACitrus.hpp"
#include "ABerry.hpp"
#include <iostream>

FruitUtils::FruitUtils()
{
}

FruitUtils::~FruitUtils()
{
}

void FruitUtils::sort(FruitBox &unsorted, FruitBox &lemons, FruitBox &citrus, FruitBox &berry)
{
    unsigned int i = 0;

    while (i < unsorted.nbFruits()) {
        IFruit *fruit = unsorted.getFruitAt(i);

        if (dynamic_cast<Lemon *>(fruit) != nullptr) {
            lemons.pushFruit(fruit);
            unsorted.popFruit();
        } else if (dynamic_cast<ACitrus *>(fruit) != nullptr) {
            citrus.pushFruit(fruit);
            unsorted.popFruit();
        } else if (dynamic_cast<ABerry *>(fruit) != nullptr) {
            berry.pushFruit(fruit);
            unsorted.popFruit();
        } else {
            ++i;
        }
    }
}

FruitBox **FruitUtils::pack(IFruit **fruits, unsigned int boxSize)
{
    unsigned int nbFruits = 0;

    while (fruits[nbFruits] != nullptr)
        ++nbFruits;

    unsigned int nbBoxes = nbFruits / boxSize + (nbFruits % boxSize == 0 ? 0 : 1);
    FruitBox **boxes = new FruitBox *[nbBoxes + 1];
    boxes[nbBoxes] = nullptr;

    for (unsigned int i = 0; i < nbBoxes; ++i)
        boxes[i] = new FruitBox(boxSize);

    for (unsigned int i = 0; i < nbFruits; ++i)
        boxes[i / boxSize]->pushFruit(fruits[i]);

    return boxes;
}

IFruit **FruitUtils::unpack(FruitBox **fruitBoxes)
{
    unsigned int nbFruits = 0;

    for (unsigned int i = 0; fruitBoxes[i] != nullptr; ++i)
        nbFruits += fruitBoxes[i]->nbFruits();

    IFruit **fruits = new IFruit *[nbFruits + 1];
    unsigned int j = 0;

    for (unsigned int i = 0; fruitBoxes[i] != nullptr; ++i) {
        while (fruitBoxes[i]->nbFruits() > 0) {
            fruits[j++] = fruitBoxes[i]->getFruitAt(0);
            fruitBoxes[i]->popFruit();
        }
        delete fruitBoxes[i];
    }

    fruits[j] = nullptr;

    return fruits;
}
