/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 5 - The Character Interface
*/

#pragma once

#include "IPotion.hpp"
#include "HealthPotion.hpp"
#include "PowerPotion.hpp"
#include "PoisonPotion.hpp"

#include <string>

class ICharacter {
    public:
        ICharacter() = default;
        virtual ~ICharacter() = default;

        virtual const std::string &getName() const = 0;
        virtual int getPower() const = 0;
        virtual int getHp() const = 0;

        virtual int attack() = 0;
        virtual int special() = 0;
        virtual void rest() = 0;

        virtual void damage(int damage) = 0;

        virtual void drink(const IPotion &potion) = 0;
        virtual void drink(const HealthPotion &potion) = 0;
        virtual void drink(const PowerPotion &potion) = 0;
        virtual void drink(const PoisonPotion &potion) = 0;
};
