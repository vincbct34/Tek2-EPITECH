/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 3 - Ponymorphism
*/

#include "Toy.hpp"

#include <iostream>

Toy::Toy()
    : _type(BASIC_TOY), _name("toy"), _picture("")
{
}

Toy::Toy(ToyType type, const std::string &name, const std::string &file)
    : _type(type), _name(name), _picture(file)
{
}

Toy::~Toy()
{
}

Toy::ToyType Toy::getType() const
{
    return this->_type;
}

std::string Toy::getName() const
{
    return this->_name;
}

void Toy::setName(const std::string &name)
{
    this->_name = name;
}

std::string Toy::getAscii() const
{
    return this->_picture.data;
}

bool Toy::setAscii(const std::string &file)
{
    return this->_picture.getPictureFromFile(file);
}

Toy &Toy::operator=(const Toy &other) {
    if (this != &other) {
        _type = other._type;
        _name = other._name;
        _picture = other._picture;
    }
    return *this;
}

void Toy::speak(const std::string &statement)
{
    std::cout << this->getName() << " \"" << statement << "\"" << std::endl;
}
