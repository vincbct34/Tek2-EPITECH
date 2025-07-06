/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** 4069.cpp
*/

#include "4069.hpp"

C4069::C4069()
{
    for (int i = 0; i < 14; i++) {
        _links[i].first = nullptr;
        _links[i].second = 0;
    }
}

C4069::~C4069()
{
}

void C4069::simulate(std::size_t tick)
{
    (void)tick;
}

nts::Tristate C4069::computeGate(AllLinks link1)
{
    if (link1.first == nullptr)
        return nts::Tristate::Undefined;
    nts::Tristate val1 = link1.first->compute(link1.second);
    if (val1 == nts::Tristate::Undefined)
        return nts::Tristate::Undefined;
    return val1 == nts::Tristate::True ? nts::Tristate::False : nts::Tristate::True;
}

nts::Tristate C4069::compute(std::size_t pin)
{
    if (pin == 2)
        return computeGate(_links[0]);
    if (pin == 4)
        return computeGate(_links[2]);
    if (pin == 6)
        return computeGate(_links[4]);
    if (pin == 8)
        return computeGate(_links[8]);
    if (pin == 10)
        return computeGate(_links[10]);
    if (pin == 12)
        return computeGate(_links[12]);
    return nts::Tristate::Undefined;
}

void C4069::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin)
{
    _links[pin - 1].first = &other;
    _links[pin - 1].second = otherPin;
}
