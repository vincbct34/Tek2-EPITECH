/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1 - KoalaBot
*/

#pragma once

#include "Parts.hpp"

class KoalaBot {
    public:
        KoalaBot(std::string serial = "Bob-01");
        ~KoalaBot();

        void setParts(const Arms &arms);
        void setParts(const Legs &legs);
        void setParts(const Head &head);

        void swapParts(Arms &arms);
        void swapParts(Legs &legs);
        void swapParts(Head &head);

        void informations() const;
        bool status() const { return _arms.isFunctionnal() && _legs.isFunctionnal() && _head.isFunctionnal(); }

    private:
        Arms _arms;
        Legs _legs;
        Head _head;
        std::string _serial;
};
