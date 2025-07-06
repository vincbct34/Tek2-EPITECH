/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** Or.hpp
*/

#pragma once

#include "IComponent.hpp"

class Or : public nts::IComponent {
    public:
        Or();
        ~Or();

        void simulate(std::size_t tick);
        nts::Tristate compute(std::size_t pin);
        void setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin);

    private:
        AllLinks _links[3];
};
