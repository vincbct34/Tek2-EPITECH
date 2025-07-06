/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 5 - Nesting
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
    if (!this->_picture.getPictureFromFile(file)) {
        this->_error.setType(Error::PICTURE);
        return false;
    }
    return true;
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

bool Toy::speak_es(const std::string &statement)
{
    (void)statement;
    this->_error.setType(Error::SPEAK);
    return false;
}

Toy &Toy::operator<<(const std::string &picture)
{
    this->_picture.data = picture;
    return *this;
}

std::string Toy::Error::what() const
{
    switch (type) {
        case UNKNOWN:
            return "";
        case SPEAK:
            return "wrong mode";
        case PICTURE:
            return "bad new illustration";
    }
    return "";
}

std::string Toy::Error::where() const
{
    switch (type) {
        case UNKNOWN:
            return "";
        case SPEAK:
            return "speak_es";
        case PICTURE:
            return "setAscii";
    }
    return "";
}

void Toy::Error::setType(ErrorType type)
{
    this->type = type;
}

Toy::Error Toy::getLastError() const
{
    return _error;
}

std::ostream &operator<<(std::ostream &os, const Toy &toy)
{
    os << toy.getName() << std::endl << toy.getAscii() << std::endl;
    return os;
}
