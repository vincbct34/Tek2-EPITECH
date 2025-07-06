/*
** EPITECH PROJECT, 2025
** NanoTekSpice
** File description:
** ConfigParser.hpp
*/

#pragma once

#include "ComponentFactory.hpp"
#include "Exceptions.hpp"
#include "Circuit.hpp"

#include <unordered_set>
#include <fstream>
#include <sstream>

class ConfigParser {
public:
    ConfigParser(const std::string &filename);

    void parse(Circuit &circuit);

private:
    std::string _filename;

    void parseChipsets(Circuit &circuit, const std::vector<std::string> &lines);
    void parseLinks(Circuit &circuit, const std::vector<std::string> &lines);
    bool isSkipped(const std::string &line);
};
