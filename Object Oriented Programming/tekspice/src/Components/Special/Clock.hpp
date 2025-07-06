/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** Clock.hpp
*/

#pragma once

#include "IComponent.hpp"

class Clock : public nts::IComponent {
    public:
        Clock();
        ~Clock();

        nts::Tristate _value = nts::Tristate::Undefined;
        nts::Tristate _tmpValue = nts::Tristate::Undefined;

        void simulate(std::size_t tick) override;
        nts::Tristate compute(std::size_t pin) override;
        void setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) override;

        void setValue(nts::Tristate value) {
            _tmpValue = value;
        }

    private:
        AllLinks _links[1];
};
