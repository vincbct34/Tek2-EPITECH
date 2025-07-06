/*
** EPITECH PROJECT, 2025
** Interstonar [WSL: Ubuntu-24.04]
** File description:
** Parser
*/

#pragma once

#include "cpptoml.h"
#include "Body.hpp"

#include <iostream>
#include <fstream>
#include <string>
#include <vector>

std::vector<Body> parseConfig(const std::string& filename);
std::vector<Body> parseLocalConfig(const std::string& filename);
