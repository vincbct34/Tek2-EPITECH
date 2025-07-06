/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** Circuit.cpp
*/

#include "Circuit.hpp"

void Circuit::addComponent(const std::string &name, std::unique_ptr<nts::IComponent> component) {
    _components[name] = std::move(component);
}

void Circuit::addLink(const std::string &first, std::size_t firstPin, const std::string &second, std::size_t secondPin) {
    _links.emplace_back(first, firstPin, second, secondPin);
}

void Circuit::simulate() {
    _tick++;

    for (auto &component : _components) {
        if (auto input = dynamic_cast<Input *>(component.second.get())) {
            if (input->_value != input->_tmpValue)
                input->_value = input->_tmpValue;
        }
        if (auto clock = dynamic_cast<Clock *>(component.second.get())) {
            clock->simulate(_tick);
        }
        if (auto output = dynamic_cast<Output *>(component.second.get())) {
            output->compute(1);
        }
    }
}

void Circuit::display() const {
    std::cout << "tick: " << _tick << std::endl;
    std::cout << "input(s):" << std::endl;

    std::vector<std::string> componentNames;
    for (const auto &component : _components) {
        componentNames.push_back(component.first);
    }
    std::sort(componentNames.begin(), componentNames.end());

    for (const auto &name : componentNames) {
        if (dynamic_cast<Input *>(_components.at(name).get()) != nullptr || dynamic_cast<Clock *>(_components.at(name).get()) != nullptr) {
            nts::Tristate value = _components.at(name)->compute(1);
            std::cout << "  " << name << ": " << (value == nts::Tristate::Undefined ? "U" : std::to_string(static_cast<int>(value))) << std::endl;
        }
    }

    std::cout << "output(s):" << std::endl;
    for (const auto &name : componentNames) {
        if (dynamic_cast<Output *>(_components.at(name).get()) != nullptr) {
            nts::Tristate value = _components.at(name)->compute(1);
            std::cout << "  " << name << ": " << (value == nts::Tristate::Undefined ? "U" : std::to_string(static_cast<int>(value))) << std::endl;
        }
    }
}

nts::IComponent *Circuit::getComponent(const std::string &name) const {
    try {
        return _components.at(name).get();
    } catch (const std::out_of_range &) {
        return nullptr;
    }
}

void Circuit::setValues(const std::string &command) {
    std::regex commandRegex(R"(\s*(\w+)\s*=\s*(\w+)\s*)");
    std::smatch match;

    if (!std::regex_match(command, match, commandRegex)) {
        std::cerr << "Error: invalid command format" << std::endl;
        return;
    }

    std::string name = match[1];
    std::string value = match[2];

    if (getComponent(name) == nullptr) {
        std::cerr << "Error: component not found" << std::endl;
        return;
    }

    if (dynamic_cast<Input *>(getComponent(name)) == nullptr && dynamic_cast<Clock *>(getComponent(name)) == nullptr) {
        std::cerr << "Error: component is not an input" << std::endl;
        return;
    }

    if (value == "1")
        if (dynamic_cast<Clock *>(getComponent(name)) != nullptr)
            dynamic_cast<Clock *>(getComponent(name))->setValue(nts::Tristate::True);
        else
            dynamic_cast<Input *>(getComponent(name))->setValue(nts::Tristate::True);
    else if (value == "0")
        if (dynamic_cast<Clock *>(getComponent(name)) != nullptr)
            dynamic_cast<Clock *>(getComponent(name))->setValue(nts::Tristate::False);
        else
            dynamic_cast<Input *>(getComponent(name))->setValue(nts::Tristate::False);
    else if (value == "U")
        if (dynamic_cast<Clock *>(getComponent(name)) != nullptr)
            dynamic_cast<Clock *>(getComponent(name))->setValue(nts::Tristate::Undefined);
        else
            dynamic_cast<Input *>(getComponent(name))->setValue(nts::Tristate::Undefined);
    else
        std::cerr << "Error: invalid value" << std::endl;
}
