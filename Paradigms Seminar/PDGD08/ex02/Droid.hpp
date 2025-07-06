/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - Roger Roger
*/

#pragma once

#include "DroidMemory.hpp"

#include <string>

class Droid {
    public:
        Droid(std::string serial);
        Droid(const Droid &replicant);
        ~Droid();

        Droid &operator=(const Droid &replicant);

        std::string getId() const;
        size_t getEnergy() const;
        size_t getAttack() const;
        size_t getToughness() const;
        std::string *getStatus() const;
        DroidMemory *getBattleData() const;

        void setId(std::string id);
        void setEnergy(size_t energy);
        void setStatus(std::string *status);
        void setBattleData(DroidMemory *battleData);

        bool operator==(const Droid &rhs) const;
        bool operator!=(const Droid &rhs) const;

        Droid &operator<<(size_t &energy);

        bool operator()(const std::string *task, size_t exp);

    private:
        std::string _id;
        size_t _remainingEnergy;
        const size_t _attackPower;
        const size_t _toughness;
        std::string *_status;
        DroidMemory *BattleData;
};

std::ostream &operator<<(std::ostream &os, const Droid &droid);
