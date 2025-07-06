/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** Output.hpp
*/

#pragma once

#include "IComponent.hpp"
#include <string>

class Output : public nts::IComponent {
    public:
        Output();
        ~Output();

        void simulate(std::size_t tick) override;
        nts::Tristate compute(std::size_t pin) override;
        void setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) override;

    private:
        AllLinks _links[1];
        nts::Tristate _value = nts::Tristate::Undefined;
};
