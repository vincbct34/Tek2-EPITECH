/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - Meeeeeeedic
*/

#pragma once

#include <string>

class Skat {
    public:
        Skat(const std::string &name = "bob", int stimPaks = 15);
        ~Skat();

        int &stimPaks();
        const std::string &name();

        void shareStimPaks(int number, int &stock);
        void addStimPaks(unsigned int number);
        void useStimPaks();
        void status();

    private:
        const std::string _name;
        int _stimPaks;
};
