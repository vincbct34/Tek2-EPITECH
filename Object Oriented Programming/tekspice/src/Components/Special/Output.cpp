/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** Output.cpp
*/

#include "Output.hpp"
#include "IComponent.hpp"

Output::Output()
{
    _links[0].first = nullptr;
    _links[0].second = 0;
}

Output::~Output()
{
}

void Output::simulate(std::size_t tick)
{
    (void)tick;
}

nts::Tristate Output::compute(std::size_t pin)
{
    if (pin == 1) {
        if (_links[0].first == nullptr)
            return nts::Tristate::Undefined;
        return _links[0].first->compute(_links[0].second);
    }
    return nts::Tristate::Undefined;
}

void Output::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin)
{
    _links[pin - 1].first = &other;
    _links[pin - 1].second = otherPin;
}
