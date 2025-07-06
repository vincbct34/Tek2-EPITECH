/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 1 - DroidMemory
*/

#pragma once

#include <cstddef>
#include <cstdlib>

class DroidMemory {
    public:
        DroidMemory();
        ~DroidMemory();

        size_t getFingerprint() const;
        size_t getExp() const;

        void setFingerprint(size_t fingerprint);
        void setExp(size_t exp);

        DroidMemory &operator<<(DroidMemory &rhs);
        DroidMemory &operator>>(DroidMemory &rhs);
        DroidMemory &operator+=(DroidMemory &rhs);
        DroidMemory &operator+=(size_t exp);
        DroidMemory &operator+(DroidMemory &rhs);
        DroidMemory &operator+(size_t exp);

    private:
        size_t _fingerprint;
        size_t _exp;      
};
