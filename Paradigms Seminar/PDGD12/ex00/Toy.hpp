/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - Encapsulation
*/

#pragma once

#include "Picture.hpp"

#include <string>

class Toy {
    public:
        enum ToyType {
            BASIC_TOY,
            ALIEN
        };

        Toy();
        Toy(ToyType type, const std::string &name, const std::string &file);
        ~Toy();

        ToyType getType() const;
        std::string getName() const;
        void setName(const std::string &name);
        std::string getAscii() const;
        bool setAscii(const std::string &file);

    protected:
        ToyType _type;
        std::string _name;
        Picture _picture;
};
