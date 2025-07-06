/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - Roger Roger
*/

#include "Droid.hpp"
#include "DroidMemory.hpp"

#include <iostream>
#include <string>

Droid::Droid(std::string serial)
    : _id(serial), _remainingEnergy(50), _attackPower(25), _toughness(15), _status(new std::string("Standing by")), BattleData(new DroidMemory())
{
    std::cout << "Droid '" << _id << "' Activated" << std::endl;
}

Droid::Droid(const Droid &replicant)
    : _id(replicant._id), _remainingEnergy(replicant._remainingEnergy), _attackPower(replicant._attackPower), _toughness(replicant._toughness), _status(new std::string(*replicant._status))
{
    std::cout << "Droid '" << _id << "' Activated, Memory Dumped" << std::endl;
}

Droid::~Droid()
{
    delete _status;

    std::cout << "Droid '" << _id << "' Destroyed" << std::endl;
}

Droid &Droid::operator=(const Droid &other)
{
    setId(other.getId());
    setStatus(new std::string(*other.getStatus()));
    setEnergy(other.getEnergy());

    return *this;
}

std::string Droid::getId() const
{
    return _id;
}

size_t Droid::getEnergy() const
{
    return _remainingEnergy;
}

size_t Droid::getAttack() const
{
    return _attackPower;
}

size_t Droid::getToughness() const
{
    return _toughness;
}

std::string *Droid::getStatus() const
{
    return _status;
}

DroidMemory *Droid::getBattleData() const
{
    return BattleData;
}

void Droid::setId(std::string id)
{
    _id = id;
}

void Droid::setEnergy(size_t energy)
{
    _remainingEnergy = energy;
}

void Droid::setStatus(std::string *status)
{
    delete _status;
    _status = status;
}

void Droid::setBattleData(DroidMemory *battleData)
{
    BattleData = battleData;
}

bool Droid::operator==(const Droid &rhs) const
{
    return *(_status) == *(rhs._status);
}

bool Droid::operator!=(const Droid &rhs) const
{
    return *(_status) != *(rhs._status);
}

Droid &Droid::operator<<(size_t &energy)
{
    size_t neededEnergy = 100 - _remainingEnergy;

    if (energy <= neededEnergy) {
        _remainingEnergy += energy;
        energy = 0;
    } else {
        _remainingEnergy += neededEnergy;
        energy -= neededEnergy;
    }
    return *this;
}

std::ostream &operator<<(std::ostream &os, const Droid &droid)
{
    os << "Droid '" << droid.getId() << "', " << *(droid.getStatus()) << ", " << droid.getEnergy();
    return os;
}

bool Droid::operator()(const std::string *task, size_t xp)
{
    if (_remainingEnergy <= 10) {
        setEnergy(0);
        setStatus(new std::string("Battery Low"));
        return false;
    }

    _remainingEnergy -= 10;
    setStatus(new std::string(*task));

    if (getBattleData()->getExp() >= xp) {
        *_status += " - Completed!";
        getBattleData()->setExp(getBattleData()->getExp() + (xp / 2));
        return true;
    }

    *_status += " - Failed!";
    getBattleData()->setExp(getBattleData()->getExp() + xp);

    return false;
}
