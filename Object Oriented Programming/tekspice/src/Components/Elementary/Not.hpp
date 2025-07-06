/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** Not.hpp
*/

#pragma once

#include "IComponent.hpp"

class Not : public nts::IComponent {
    public:
        Not();
        ~Not();

        void simulate(std::size_t tick);
        nts::Tristate compute(std::size_t pin);
        void setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin);

    private:
        AllLinks _links[2];
};