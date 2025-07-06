/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** Not.cpp
*/

#include "Not.hpp"

Not::Not()
{
    _links[0].first = nullptr;
    _links[0].second = 0;

    _links[1].first = nullptr;
    _links[1].second = 0;
}

Not::~Not()
{
}

void Not::simulate(std::size_t tick)
{
    (void)tick;
}

nts::Tristate Not::compute(std::size_t pin)
{
    if (pin == 2) {
        if (_links[0].first == nullptr)
            return nts::Tristate::Undefined;
        nts::Tristate val1 = _links[0].first->compute(_links[0].second);
        if (val1 == nts::Tristate::Undefined)
            return nts::Tristate::Undefined;
        return (val1 == nts::Tristate::True) ? nts::Tristate::False : nts::Tristate::True;
    }
    return nts::Tristate::Undefined;
}

void Not::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin)
{
    _links[pin - 1].first = &other;
    _links[pin - 1].second = otherPin;
}