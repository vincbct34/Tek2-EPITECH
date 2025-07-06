/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 3 - Ponymorphism
*/

#pragma once

#include <string>

class Picture {
    public:
        Picture();
        Picture(const std::string &file);
        ~Picture();

        std::string data;

        bool getPictureFromFile(const std::string &file);

        Picture &operator=(const Picture &other);
};
