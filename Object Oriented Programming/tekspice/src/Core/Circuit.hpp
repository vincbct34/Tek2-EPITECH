/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** Circuit.hpp
*/

#pragma once

#include "IComponent.hpp"
#include "Exceptions.hpp"
#include "Output.hpp"
#include "Input.hpp"
#include "Clock.hpp"

#include <unordered_map>
#include <iostream>
#include <cstddef>
#include <regex>
#include <vector>
#include <memory>

class Circuit {
public:
    void addLink(const std::string &first, std::size_t firstPin, const std::string &second, std::size_t secondPin);
    void addComponent(const std::string &name, std::unique_ptr<nts::IComponent> component);

    void display() const;
    void simulate();

    int getValue(const std::string &name) const;
    void setValues(const std::string &command);

    const std::unordered_map<std::string, std::unique_ptr<nts::IComponent>> &getComponents() const {
        return _components;
    };
    const std::vector<std::tuple<std::string, std::size_t, std::string, std::size_t>> &getLinks() const {
        return _links;
    };

    nts::IComponent *getComponent(const std::string &name) const;

private:
    std::vector<std::tuple<std::string, std::size_t, std::string, std::size_t>> _links;
    std::unordered_map<std::string, std::unique_ptr<nts::IComponent>> _components;
    size_t _tick = 0;
};
