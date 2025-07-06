/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** Or.cpp
*/

#include "Or.hpp"
#include "IComponent.hpp"

Or::Or()
{
    _links[0].first = nullptr;
    _links[0].second = 0;

    _links[1].first = nullptr;
    _links[1].second = 0;

    _links[2].first = nullptr;
    _links[2].second = 0;
}

Or::~Or()
{
}

void Or::simulate(std::size_t tick)
{
    (void)tick;
}

nts::Tristate Or::compute(std::size_t pin)
{
    if (pin == 3) {
        if (_links[0].first == nullptr || _links[1].first == nullptr)
            return nts::Tristate::Undefined;
        nts::Tristate val1 = _links[0].first->compute(_links[0].second);
        nts::Tristate val2 = _links[1].first->compute(_links[1].second);
        if (val1 == nts::Tristate::True || val2 == nts::Tristate::True)
            return nts::Tristate::True;
        else if (val1 == nts::Tristate::False && val2 == nts::Tristate::False)
            return nts::Tristate::False;
        return nts::Tristate::Undefined;
    }
    return nts::Tristate::Undefined;
}

void Or::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin)
{
    _links[pin - 1].first = &other;
    _links[pin - 1].second = otherPin;
}
