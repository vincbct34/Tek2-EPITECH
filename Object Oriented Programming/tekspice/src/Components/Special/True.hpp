/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** True.hpp
*/

#pragma once

#include "IComponent.hpp"

class True : public nts::IComponent {
    public:
        True();
        ~True();
        
        void simulate(std::size_t tick) override;
        nts::Tristate compute(std::size_t pin) override;
        void setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) override;
};
