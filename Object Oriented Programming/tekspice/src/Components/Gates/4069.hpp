/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** 4069.hpp
*/

#pragma once

#include "IComponent.hpp"

class C4069 : public nts::IComponent {
    public:
        C4069();
        ~C4069();

        void simulate(std::size_t tick);
        nts::Tristate compute(std::size_t pin);
        nts::Tristate computeGate(AllLinks link1);
        void setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin);

    private:
        AllLinks _links[14];
};
