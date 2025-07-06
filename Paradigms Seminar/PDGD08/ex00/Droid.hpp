/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - Droids
*/

#pragma once

#include <string>

class Droid {
    public:
        Droid(const std::string serial = "");
        Droid(const Droid &replicant);
        ~Droid();

        Droid &operator=(const Droid &replicant);

        std::string getId() const;
        size_t getEnergy() const;
        size_t getAttack() const;
        size_t getToughness() const;
        std::string *getStatus() const;

        void setId(std::string id);
        void setEnergy(size_t energy);
        void setStatus(std::string *status);

        bool operator==(const Droid &rhs) const;
        bool operator!=(const Droid &rhs) const;

        Droid &operator<<(size_t &energy);

    private:
        std::string _id;
        size_t _remainingEnergy;
        const size_t _attackPower;
        const size_t _toughness;
        std::string *_status;
};

std::ostream &operator<<(std::ostream &os, const Droid &droid);
