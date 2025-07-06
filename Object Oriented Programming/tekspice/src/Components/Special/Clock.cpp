/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** Clock.cpp
*/

#include "Clock.hpp"
#include "IComponent.hpp"

#include <iostream>

Clock::Clock()
{
    _links[0].first = nullptr;
    _links[0].second = 0;
}

Clock::~Clock()
{
}

void Clock::simulate(std::size_t tick)
{
    (void)tick;

    if (_tmpValue == nts::Undefined) {
        if (_value == nts::Tristate::True)
            _value = nts::Tristate::False;
        else if (_value == nts::Tristate::False)
            _value = nts::Tristate::True;
        _value = _tmpValue;
    }
    if (_tmpValue != nts::Undefined) {
        _value = _tmpValue;
        _tmpValue = (_value == nts::Tristate::True) ? nts::Tristate::False : nts::Tristate::True;
    }
}

nts::Tristate Clock::compute(std::size_t pin)
{
    if (pin == 1) {
        if (_links[0].first == nullptr)
            return nts::Tristate::Undefined;
        return _value;
    }
    return nts::Tristate::Undefined;
}

void Clock::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin)
{
    _links[pin - 1].first = &other;
    _links[pin - 1].second = otherPin;
}
