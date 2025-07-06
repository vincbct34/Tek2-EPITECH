/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1 - KoalaBot
*/

#pragma once

#include <string>

class Arms {
    public:
        Arms(std::string serial = "A-01", bool functional = true);
        ~Arms();
        bool isFunctionnal() const { return _functional; }
        std::string serial() const { return _serial; }
        void informations() const;

    private:
        std::string _serial;
        bool _functional;
};

class Legs {
    public:
        Legs(std::string serial = "L-01", bool functional = true);
        ~Legs();
        bool isFunctionnal() const { return _functional; }
        std::string serial() const { return _serial; }
        void informations() const;

    private:
        std::string _serial;
        bool _functional;
};

class Head {
    public:
        Head(std::string serial = "H-01", bool functional = true);
        ~Head();
        bool isFunctionnal() const { return _functional; }
        std::string serial() const { return _serial; }
        void informations() const;

    private:
        std::string _serial;
        bool _functional;
};
