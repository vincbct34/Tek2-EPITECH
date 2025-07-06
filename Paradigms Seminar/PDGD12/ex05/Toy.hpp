/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 5 - Nesting
*/

#pragma once

#include "Picture.hpp"

#include <string>

class Toy {
    public:
        class Error {
            public:
                enum ErrorType {
                    UNKNOWN,
                    SPEAK,
                    PICTURE
                };

                ErrorType type;

                std::string what() const;
                std::string where() const;

                void setType(ErrorType type);
        };

        enum ToyType {
            BASIC_TOY,
            ALIEN,
            BUZZ,
            WOODY
        };

        Toy();
        Toy(ToyType type, const std::string &name, const std::string &file);
        ~Toy();

        ToyType getType() const;
        std::string getName() const;
        void setName(const std::string &name);
        std::string getAscii() const;
        bool setAscii(const std::string &file);

        Toy &operator=(const Toy &other);

        virtual void speak(const std::string &statement);

        Toy &operator<<(const std::string &picture);

        virtual bool speak_es(const std::string &statement);

        Error getLastError() const;

    protected:
        ToyType _type;
        std::string _name;
        Picture _picture;
        Error _error;
};

std::ostream &operator<<(std::ostream &os, const Toy &toy);
