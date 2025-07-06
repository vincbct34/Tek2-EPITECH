/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** ComponentFactory.hpp
*/

#pragma once

#include "IComponent.hpp"

#include "Or.hpp"
#include "Input.hpp"
#include "Output.hpp"
#include "And.hpp"
#include "Xor.hpp"
#include "Not.hpp"
#include "Clock.hpp"
#include "True.hpp"
#include "False.hpp"
#include "4001.hpp"
#include "4011.hpp"
#include "4030.hpp"
#include "4069.hpp"
#include "4071.hpp"
#include "4081.hpp"

#include <functional>
#include <memory>

class ComponentFactory {
public:
    static std::unique_ptr<nts::IComponent> createComponent(const std::string &type);

    void setAllLinks(const std::string &name, Circuit *circuit);

private:
    static std::unordered_map<std::string, std::function<std::unique_ptr<nts::IComponent>()>> _factory;
};
