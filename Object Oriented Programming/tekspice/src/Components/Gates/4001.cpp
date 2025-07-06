/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** 4001.cpp
*/

#include "4001.hpp"
#include "Exceptions.hpp"

C4001::C4001()
{
    for (int i = 0; i < 14; i++) {
        _links[i].first = nullptr;
        _links[i].second = 0;
    }
}

C4001::~C4001()
{
}

void C4001::simulate(std::size_t tick)
{
    (void)tick;
}

nts::Tristate C4001::computeGate(AllLinks link1, AllLinks link2)
{
    if (link1.first == nullptr || link2.first == nullptr)
        return nts::Tristate::Undefined;
    nts::Tristate val1 = link1.first->compute(link1.second);
    nts::Tristate val2 = link2.first->compute(link2.second);
    if (val1 == nts::Tristate::False && val2 == nts::Tristate::False)
        return nts::Tristate::True;
    if (val1 == nts::Tristate::True || val2 == nts::Tristate::True)
        return nts::Tristate::False;
    return nts::Tristate::Undefined;
}

nts::Tristate C4001::compute(std::size_t pin)
{
    if (pin == 3)
        return computeGate(_links[0], _links[1]);
    if (pin == 4)
        return computeGate(_links[4], _links[5]);
    if (pin == 10)
        return computeGate(_links[7], _links[8]);
    if (pin == 11)
        return computeGate(_links[11], _links[12]);
    return nts::Tristate::Undefined;
}

void C4001::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin)
{
        if (pin > 14 || _links[pin - 1].first != nullptr) {
        throw ComponentTypeError("4001");
    }
    _links[pin - 1].first = &other;
    _links[pin - 1].second = otherPin;
}
