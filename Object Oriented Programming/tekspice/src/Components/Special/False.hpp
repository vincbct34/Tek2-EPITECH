/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** False.hpp
*/

#pragma once

#include "IComponent.hpp"

class False : public nts::IComponent {
    public:
        False();
        ~False();
        
        void simulate(std::size_t tick) override;
        nts::Tristate compute(std::size_t pin) override;
        void setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) override;
};
