/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** And.hpp
*/

#pragma once

#include "IComponent.hpp"

class And : public nts::IComponent {
    public:
        And();
        ~And();

        void simulate(std::size_t tick);
        nts::Tristate compute(std::size_t pin);
        void setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin);

    private:
        AllLinks _links[3];
};