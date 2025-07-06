/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1 - The Fruit Box
*/

#include "FruitBox.hpp"

FruitBox::FruitBox(unsigned int size)
    : _size(size), _fruits()
{
}

FruitBox::~FruitBox()
{
    while (!_fruits.empty()) {
        delete _fruits.front();
        _fruits.erase(_fruits.begin());
    }
}

unsigned int FruitBox::getSize() const
{
    return _size;
}

unsigned int FruitBox::nbFruits() const
{
    return _fruits.size();
}

bool FruitBox::pushFruit(IFruit *fruit)
{
    if (_fruits.size() < _size) {
        _fruits.push_back(fruit);
        return true;
    }
    return false;
}

IFruit *FruitBox::popFruit()
{
    if (_fruits.empty())
        return nullptr;
    IFruit *fruit = _fruits.front();
    _fruits.erase(_fruits.begin());
    return fruit;
}

IFruit *FruitBox::getFruitAt(unsigned int idx) const
{
    if (idx < _fruits.size())
        return _fruits[idx];
    return nullptr;
}

std::ostream &operator<<(std::ostream &s, FruitBox const &box)
{
    s << "[";
    unsigned int count = box.nbFruits();
    for (unsigned int i = 0; i < count; ++i)
    {
        IFruit *fruit = box.getFruitAt(i);
        if (fruit)
        {
            s << "[name: \"" << fruit->getName()
              << "\", vitamins: " << fruit->getVitamins()
              << ", peeled: " << (fruit->isPeeled() ? "true" : "false") << "]";
            if (i < count - 1)
                s << ", ";
        }
    }
    s << "]";
    return s;
}
