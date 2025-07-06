/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** 4081.hpp
*/

#pragma once

#include "IComponent.hpp"

class C4081 : public nts::IComponent {
    public:
        C4081();
        ~C4081();

        void simulate(std::size_t tick);
        nts::Tristate compute(std::size_t pin);
        nts::Tristate computeGate(AllLinks link1, AllLinks link2);
        void setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin);

    private:
        AllLinks _links[14];
};