/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - Simple inheritance
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
