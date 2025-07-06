/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Packet
*/

#pragma once

#include <sstream>
#include <string>
#include <vector>

struct Packet {
    std::string type;
    std::vector<std::string> args;

    static Packet parse(const std::string& raw);
    std::string serialize() const;
};
