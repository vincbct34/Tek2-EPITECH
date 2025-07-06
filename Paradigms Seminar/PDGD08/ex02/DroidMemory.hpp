/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - Roger Roger
*/

#pragma once

#include <iostream>

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

        bool operator==(DroidMemory &rhs) const;
        bool operator!=(DroidMemory &rhs) const;
        bool operator<(DroidMemory &rhs) const;
        bool operator>(DroidMemory &rhs) const;
        bool operator<=(DroidMemory &rhs) const;
        bool operator>=(DroidMemory &rhs) const;

        bool operator==(size_t) const;
        bool operator!=(size_t) const;
        bool operator<(size_t) const;
        bool operator>(size_t) const;

    private:
        size_t _fingerprint;
        size_t _exp;        
};

std::ostream &operator<<(std::ostream &os, const DroidMemory &memory);
