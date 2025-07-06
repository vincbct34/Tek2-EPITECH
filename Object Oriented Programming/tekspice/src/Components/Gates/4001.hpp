/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** 4001.hpp
*/

#pragma once

#include "IComponent.hpp"

class C4001 : public nts::IComponent {
    public:
        C4001();
        ~C4001();

        void simulate(std::size_t tick);
        nts::Tristate compute(std::size_t pin);
        nts::Tristate computeGate(AllLinks link1, AllLinks link2);
        void setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin);

    private:
        AllLinks _links[14];
};
