/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** ConfigParser.cpp
*/

#include "ConfigParser.hpp"

ConfigParser::ConfigParser(const std::string &filename)
    : _filename(filename)
{
}

std::ifstream checkFile(const std::string &filename)
{
    std::ifstream file;

    file.open(filename);
    if (!file.is_open())
        throw FileError("Error: file does not exist");

    if (filename.substr(filename.find_last_of(".") + 1) != "nts")
        throw FileError("Error: file is not a .nts file");

    return file;
}

bool ConfigParser::isSkipped(const std::string &line) {
    if (line.empty() || line.find('#') == 0)
        return true;

    // Check if the line contains only spaces or tabs
    if (line.find_first_not_of(" \t") == std::string::npos)
        return true;

    return false;
}

std::string trim(const std::string &str) {
    size_t first = str.find_first_not_of(" \t");
    if (first == std::string::npos)
        return "";
    size_t last = str.find_last_not_of(" \t");
    return str.substr(first, last - first + 1);
}

void ConfigParser::parse(Circuit &circuit) {
    std::ifstream file = checkFile(_filename);

    std::vector<std::string> chipsets;
    std::vector<std::string> links;

    std::string line;

    enum Section {
        CHIPSETS,
        LINKS,
        NONE
    } section = NONE;

    while (std::getline(file, line)) {
        line = trim(line); // Trim spaces and tabs

        if (line == ".chipsets:") {
            section = CHIPSETS;
            continue;
        } else if (line == ".links:") {
            section = LINKS;
            continue;
        }

        if (section == CHIPSETS && !isSkipped(line)) {
            chipsets.push_back(line);
        } else if (section == LINKS && !isSkipped(line)) {
            links.push_back(line);
        }
    }

    if (chipsets.empty())
        throw NoChipsetsError();

    if (links.empty())
        throw NoLinksError();

    parseChipsets(circuit, chipsets);
    parseLinks(circuit, links);
}

void ConfigParser::parseChipsets(Circuit &circuit, const std::vector<std::string> &lines) {
    std::unordered_set<std::string> componentNames;

    for (const auto &line : lines) {
        std::istringstream iss(trim(line)); // Trim spaces and tabs
        std::string type, name;

        if (!(iss >> type >> name)) {
            throw SyntaxicError("chipsets should be formatted as: <type> <name>");
        }

        type = trim(type); // Ensure type is trimmed
        name = trim(name); // Ensure name is trimmed

        if (componentNames.find(name) != componentNames.end()) {
            throw DuplicateComponentError(name);
        }

        componentNames.insert(name);

        auto component = ComponentFactory::createComponent(type);

        if (!component) {
            throw ComponentTypeError(type);
        }

        circuit.addComponent(name, std::move(component));
    }
}

void ConfigParser::parseLinks(Circuit &circuit, const std::vector<std::string> &lines) {
    (void)circuit;
    for (const auto &line : lines) {
        std::istringstream iss(trim(line)); // Trim spaces and tabs
        std::string comp1, pin1, comp2, pin2;

        if (!(std::getline(iss, comp1, ':')
            && std::getline(iss, pin1, ' ')
            && std::getline(iss, comp2, ':')
            && std::getline(iss, pin2))) {
            throw SyntaxicError("links should be formatted as: <component>:<pin> <component>:<pin>");
        }

        comp1 = trim(comp1); // Ensure comp1 is trimmed
        pin1 = trim(pin1);   // Ensure pin1 is trimmed
        comp2 = trim(comp2); // Ensure comp2 is trimmed
        pin2 = trim(pin2);   // Ensure pin2 is trimmed

        if (circuit.getComponent(comp1) == nullptr) {
            throw ComponentNameError(comp1);
        }

        if (circuit.getComponent(comp2) == nullptr) {
            throw ComponentNameError(comp2);
        }

        circuit.addLink(comp1, std::stoul(pin1), comp2, std::stoul(pin2));
    }
}
