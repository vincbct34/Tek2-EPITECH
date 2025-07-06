/*
** EPITECH PROJECT, 2025
** Interstonar [WSL: Ubuntu-24.04]
** File description:
** Simulation
*/

#pragma once

#include "Exceptions.hpp"
#include "Vector3.hpp"
#include "Parser.hpp"
#include "Body.hpp"

#include <string>

const double GRAVIT_VAR = 6.674e-11; // Constante gravitationnelle
const double DELTA_T = 3600; // 1 heure en secondes
const double MAX_STEPS = 8760; // 1 année en heures

void simulateGlobal(const std::string& configPath, const Vector3& position, const Vector3& velocity);
void simulateLocal(const std::string& configPath, const Vector3& position, const Vector3& velocity);
