/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - The Peasant
*/

#pragma once

#include "ICharacter.hpp"

#include <string>

class Peasant : public ICharacter {
    public:
        Peasant(const std::string &name, int power);
        ~Peasant();

        const std::string &getName() const override;
        int getPower() const override;
        int getHp() const override;

        virtual int attack() override;
        virtual int special() override;
        virtual void rest() override;

        void damage(int damage) override;

        virtual void drink(const IPotion &potion) override;
        virtual void drink(const HealthPotion &potion) override;
        virtual void drink(const PowerPotion &potion) override;
        virtual void drink(const PoisonPotion &potion) override;

    protected:
        std::string name;
        int powerPoints;
        int hpPoints;
};
