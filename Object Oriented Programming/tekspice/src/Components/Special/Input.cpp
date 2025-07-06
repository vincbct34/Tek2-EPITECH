/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** Input.cpp
*/

#include "Input.hpp"
#include "IComponent.hpp"

Input::Input()
{
    _links[0].first = nullptr;
    _links[0].second = 0;
}

Input::~Input()
{
}

void Input::simulate(std::size_t tick)
{
    (void)tick;
}

nts::Tristate Input::compute(std::size_t pin)
{
    if (pin == 1) {
        if (_links[0].first == nullptr)
            return nts::Tristate::Undefined;
        return _value;
    }
    return nts::Tristate::Undefined;
}

void Input::setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin)
{
    _links[pin - 1].first = &other;
    _links[pin - 1].second = otherPin;
}
