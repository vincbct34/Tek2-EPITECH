/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** 4030.hpp
*/

#pragma once

#include "IComponent.hpp"

class C4030 : public nts::IComponent {
    public:
        C4030();
        ~C4030();

        void simulate(std::size_t tick);
        nts::Tristate compute(std::size_t pin);
        nts::Tristate computeGate(AllLinks link1, AllLinks link2);
        void setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin);

    private:
        AllLinks _links[14];
};