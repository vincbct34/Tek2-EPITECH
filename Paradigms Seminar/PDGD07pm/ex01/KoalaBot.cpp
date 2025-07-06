/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1 - KoalaBot
*/

#include "KoalaBot.hpp"

#include <iostream>

KoalaBot::KoalaBot(std::string serial)
    : _serial(serial)
{
}

KoalaBot::~KoalaBot()
{
}

void KoalaBot::setParts(const Arms &arms)
{
    _arms = arms;
}

void KoalaBot::setParts(const Legs &legs)
{
    _legs = legs;
}

void KoalaBot::setParts(const Head &head)
{
    _head = head;
}

void KoalaBot::swapParts(Arms &arms)
{
    Arms tmp = _arms;

    _arms = arms;
    arms = tmp;
}

void KoalaBot::swapParts(Legs &legs)
{
    Legs tmp = _legs;

    _legs = legs;
    legs = tmp;
}

void KoalaBot::swapParts(Head &head)
{
    Head tmp = _head;

    _head = head;
    head = tmp;
}

void KoalaBot::informations() const
{
    std::cout << "[KoalaBot] " << _serial << std::endl;
    std::cout << "\t";
    _arms.informations();
    std::cout << "\t";
    _legs.informations();
    std::cout << "\t";
    _head.informations();
}
