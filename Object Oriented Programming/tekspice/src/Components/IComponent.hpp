/*
** EPITECH PROJECT, 2025
** Bootstrap NanoTekSpice
** File description:
** IComponent.hpp
*/

#pragma once

#include <utility>
#include <cstddef>
#include <string>

namespace nts
{
    enum Tristate {
        Undefined = (-true),
        True = true,
        False = false
    };

    class IComponent {
    public:
        // Destructor
        virtual                 ~IComponent() = default;

        // Member functions
        virtual void            simulate(std::size_t tick) = 0;
        virtual nts::Tristate   compute(std::size_t pin) = 0;
        virtual void            setLink(std::size_t pin, nts::IComponent &other, std::size_t otherPin) = 0;

        // Alias for a pair containing a component and its pin
        using AllLinks = std::pair<IComponent*, std::size_t>;
    };
}
