/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - Roger Roger
*/

#include "DroidMemory.hpp"

#include <iostream>

DroidMemory::DroidMemory()
    : _fingerprint(random()), _exp(0)
{
}

DroidMemory::~DroidMemory()
{
}

size_t DroidMemory::getFingerprint() const
{
    return _fingerprint;
}

size_t DroidMemory::getExp() const
{
    return _exp;
}

void DroidMemory::setFingerprint(size_t fingerprint)
{
    _fingerprint = fingerprint;
}

void DroidMemory::setExp(size_t exp)
{
    _exp = exp;
}

DroidMemory &DroidMemory::operator<<(DroidMemory &rhs)
{
    _exp += rhs.getExp();
    _fingerprint ^= rhs.getFingerprint();

    return *this;
}

DroidMemory &DroidMemory::operator>>(DroidMemory &rhs)
{
    rhs.setExp(rhs.getExp() + _exp);
    rhs.setFingerprint(rhs.getFingerprint() ^ _fingerprint);

    return *this;
}

DroidMemory &DroidMemory::operator+=(DroidMemory &rhs)
{
    _exp += rhs.getExp();
    _fingerprint ^= rhs.getFingerprint();

    return *this;
}

DroidMemory &DroidMemory::operator+=(size_t exp)
{
    _exp += exp;
    _fingerprint ^= exp;

    return *this;
}

DroidMemory &DroidMemory::operator+(DroidMemory &rhs)
{
    DroidMemory *newMemory = new DroidMemory();

    newMemory->setExp(_exp + rhs.getExp());
    newMemory->setFingerprint(_fingerprint ^ rhs.getFingerprint());

    return *newMemory;
}

DroidMemory &DroidMemory::operator+(size_t exp)
{
    DroidMemory *newMemory = new DroidMemory();

    newMemory->setExp(_exp + exp);
    newMemory->setFingerprint(_fingerprint ^ exp);

    return *newMemory;
}

std::ostream &operator<<(std::ostream &os, const DroidMemory &memory)
{
    os << "DroidMemory '" << memory.getFingerprint() << "', " << memory.getExp();
    return os;
}

bool DroidMemory::operator==(DroidMemory &rhs) const
{
    return _fingerprint == rhs.getFingerprint() && _exp == rhs.getExp();
}

bool DroidMemory::operator!=(DroidMemory &rhs) const
{
    return _fingerprint != rhs.getFingerprint() || _exp != rhs.getExp();
}

bool DroidMemory::operator<(DroidMemory &rhs) const
{
    return _exp < rhs.getExp();
}

bool DroidMemory::operator>(DroidMemory &rhs) const
{
    return _exp > rhs.getExp();
}

bool DroidMemory::operator<=(DroidMemory &rhs) const
{
    return _exp <= rhs.getExp();
}

bool DroidMemory::operator>=(DroidMemory &rhs) const
{
    return _exp >= rhs.getExp();
}

bool DroidMemory::operator==(size_t exp) const
{
    return _exp == exp;
}

bool DroidMemory::operator!=(size_t exp) const
{
    return _exp != exp;
}

bool DroidMemory::operator<(size_t exp) const
{
    return _exp < exp;
}

bool DroidMemory::operator>(size_t exp) const
{
    return _exp > exp;
}
