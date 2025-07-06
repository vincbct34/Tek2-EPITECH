/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 0 - The Federation
*/

#include "Federation.hpp"
#include "WarpSystem.hpp"

#include <iostream>

Federation::Starfleet::Ship::Ship(int length, int width, std::string name, short maxWarp, int photonTorpedo)
    : _length(length), _width(width), _name(name), _maxWarp(maxWarp), _core(nullptr), _captain(nullptr), _home(EARTH), _location(EARTH), _shield(100), _photonTorpedo(photonTorpedo)
{
	std::cout << "The ship USS " << name << " has been finished." << std::endl;
    std::cout << "It is " << length << " m in length and " << width << " m in width." << std::endl;
    std::cout << "It can go to Warp " << maxWarp << "!" << std::endl;

    if (photonTorpedo && photonTorpedo != 0)
        std::cout << "Weapons are set: " << photonTorpedo << " torpedoes ready." << std::endl;
}

Federation::Starfleet::Ship::~Ship()
{
}

void Federation::Starfleet::Ship::setupCore(WarpSystem::Core *core)
{
    std::cout << "USS " << _name << ": The core is set." << std::endl;
    _core = core;
}

void Federation::Starfleet::Ship::checkCore()
{
    if (_core->checkReactor()->isStable())
        std::cout << "USS " << _name << ": The core is stable at the time." << std::endl;
    else
        std::cout << "USS " << _name << ": The core is unstable at the time." << std::endl;
}

Federation::Ship::Ship(int length, int width, std::string name)
    : _length(length), _width(width), _name(name), _core(nullptr), _home(VULCAN), _location(VULCAN)
{
    std::cout << "The independent ship " << name << " just finished its construction." << std::endl;
    std::cout << "It is " << length << " m in length and " << width << " m in width." << std::endl;
}

Federation::Ship::~Ship()
{
}

void Federation::Ship::setupCore(WarpSystem::Core *core)
{
    _core = core;
    std::cout << _name << ": The core is set." << std::endl;
}

void Federation::Ship::checkCore()
{
    if (_core && _core->checkReactor()->isStable())
        std::cout << _name << ": The core is stable at the time." << std::endl;
    else
        std::cout << _name << ": The core is unstable at the time." << std::endl;
}

Federation::Starfleet::Captain::Captain(std::string name)
    : _name(name)
{
}

Federation::Starfleet::Captain::~Captain()
{
}

std::string Federation::Starfleet::Captain::getName()
{
    return _name;
}

int Federation::Starfleet::Captain::getAge()
{
    return _age;
}

void Federation::Starfleet::Captain::setAge(int age)
{
    _age = age;
}

void Federation::Starfleet::Ship::promote(Captain *captain)
{
    _captain = captain;
    std::cout << captain->getName() << ": I'm glad to be the captain of the USS " << _name << "." << std::endl;
}

Federation::Starfleet::Ensign::Ensign(std::string name)
    : _name(name)
{
    std::cout << "Ensign " << name << ", awaiting orders." << std::endl;
}

Federation::Starfleet::Ensign::~Ensign()
{
}

bool Federation::Starfleet::Ship::move(int warp, Destination d)
{
    if (warp <= _maxWarp && d != _location && _core->checkReactor()->isStable()) {
        _location = d;
        return true;
    }
    return false;
}

bool Federation::Starfleet::Ship::move(int warp)
{
    if (warp <= _maxWarp && _core->checkReactor()->isStable()) {
        _location = _home;
        return true;
    }
    return false;
}

bool Federation::Starfleet::Ship::move(Destination d)
{
    if (d != _location && _core->checkReactor()->isStable()) {
        _location = d;
        return true;
    }
    return false;
}

bool Federation::Starfleet::Ship::move()
{
    if (_core->checkReactor()->isStable()) {
        _location = _home;
        return true;
    }
    return false;
}

bool Federation::Ship::move(int warp, Destination d)
{
    if (warp <= _maxWarp && d != _location && _core->checkReactor()->isStable()) {
        _location = d;
        return true;
    }
    return false;
}

bool Federation::Ship::move(int warp)
{
    if (warp <= _maxWarp && _core->checkReactor()->isStable()) {
        _location = _home;
        return true;
    }
    return false;
}

bool Federation::Ship::move(Destination d)
{
    if (d != _location && _core->checkReactor()->isStable()) {
        _location = d;
        return true;
    }
    return false;
}

bool Federation::Ship::move()
{
    if (_core->checkReactor()->isStable()) {
        _location = _home;
        return true;
    }
    return false;
}

int Federation::Starfleet::Ship::getShield()
{
    return _shield;
}

void Federation::Starfleet::Ship::setShield(int shield)
{
    _shield = shield;
}

int Federation::Starfleet::Ship::getTorpedo()
{
    return _photonTorpedo;
}

void Federation::Starfleet::Ship::setTorpedo(int torpedo)
{
    _photonTorpedo = torpedo;
}

void Federation::Starfleet::Ship::fire(Borg::Ship *target)
{
    if (_photonTorpedo == 0) {
        std::cout << _name << ": No more torpedo to fire, " << (_captain ?  _captain->getName() : "") << "!" << std::endl;
        return;
    }

    _photonTorpedo--;
    std::cout << _name << ": Firing on target. " << _photonTorpedo << " torpedoes remaining." << std::endl;

    target->setShield(target->getShield() - 50);    
}

void Federation::Starfleet::Ship::fire(int torpedoes, Borg::Ship *target)
{
    if (_photonTorpedo == 0) {
        std::cout << _name << ": No more torpedo to fire, " << (_captain ?  _captain->getName() : "") << "!" << std::endl;
        return;
    }

    if (_photonTorpedo < torpedoes) {
        std::cout << _name << ": Not enough torpedoes to fire, " << (_captain ?  _captain->getName() : "") << "!" << std::endl;
        return;
    }

    _photonTorpedo -= torpedoes;
    std::cout << _name << ": Firing on target. " << torpedoes << " torpedoes remaining." << std::endl;

    target->setShield(target->getShield() - 50 * torpedoes);
}
