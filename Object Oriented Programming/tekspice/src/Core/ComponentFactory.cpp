/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** ComponentFactory.cpp
*/

#include "ComponentFactory.hpp"

std::unordered_map<std::string, std::function<std::unique_ptr<nts::IComponent>()>> ComponentFactory::_factory = {
    {"input", []() { return std::make_unique<Input>(); }},
    {"output", []() { return std::make_unique<Output>(); }},
    {"or", []() { return std::make_unique<Or>(); }},
    {"and", []() { return std::make_unique<And>(); }},
    {"xor", []() { return std::make_unique<Xor>(); }},
    {"not", []() { return std::make_unique<Not>(); }},
    {"clock", []() { return std::make_unique<Clock>(); }},
    {"true", []() { return std::make_unique<True>(); }},
    {"false", []() { return std::make_unique<False>(); }},
    {"4001", []() { return std::make_unique<C4001>(); }},
    {"4011", []() { return std::make_unique<C4011>(); }},
    {"4030", []() { return std::make_unique<C4030>(); }},
    {"4069", []() { return std::make_unique<C4069>(); }},
    {"4071", []() { return std::make_unique<C4071>(); }},
    {"4081", []() { return std::make_unique<C4081>(); }}
};

std::unique_ptr<nts::IComponent> ComponentFactory::createComponent(const std::string &type)
{
    if (_factory.find(type) == _factory.end())
        return nullptr;
    return _factory[type]();
}

void ComponentFactory::setAllLinks(const std::string &name, Circuit *circuit) {
    for (const auto &[comp1, pin1, comp2, pin2] : circuit->getLinks()) {
        auto component1 = circuit->getComponent(comp1);
        auto component2 = circuit->getComponent(comp2);

        if (!component1 || !component2) {
            continue;
        }

        if (comp1 == name) {
            component1->setLink(pin1, *component2, pin2);
            component2->setLink(pin2, *component1, pin1);
        }
    }
}
