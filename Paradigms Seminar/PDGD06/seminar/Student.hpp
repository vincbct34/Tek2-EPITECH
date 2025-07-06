/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 3
*/

#pragma once

#include <string>

class Student {
    private:
        std::string name;
        int energy = 100;

    public:
        Student(std::string name);
        ~Student();

        bool learn(std::string text);
        void drink(std::string drink);

        std::string getName();
        int getEnergy();
};
