/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 3 - Lock'n load, baby
*/

#include "Phaser.hpp"
#include "Sounds.hpp"

#include <iostream>
#include <cctype>

Phaser::Phaser(int maxAmmo, AmmoType type)
    : m_maxAmmo(maxAmmo)
{
    for (int i = 0; i < 3; i++)
        m_currentAmmo[i] = maxAmmo;

    if (type != ROCKET && type != PLASMA)
        m_type = REGULAR;
    else
        m_type = type;
}

Phaser::~Phaser()
{
}

void Phaser::fire()
{
    if (m_currentAmmo[m_type] == Empty) {
        std::cout << "Clip empty, please reload" << std::endl;
        return;
    }

    if (m_type == REGULAR)
        std::cout << Sounds::Regular << "\n";
    else if (m_type == ROCKET)
        std::cout << Sounds::Rocket << "\n";
    else if (m_type == PLASMA)
        std::cout << Sounds::Plasma << "\n";

    m_currentAmmo[m_type] -= 1;
}

void Phaser::ejectClip()
{
    m_currentAmmo[m_type] = Empty;
}

void Phaser::changeType(AmmoType newType)
{
    if (newType != ROCKET && newType != PLASMA && newType != REGULAR)
        return;

    m_type = newType;
    std::cout << "Switching ammo to type: " << tolower(m_type) << std::endl;
}

void Phaser::reload()
{
    std::cout << "Reloading..." << std::endl;
    m_currentAmmo[m_type] = m_maxAmmo;
}

void Phaser::addAmmo(AmmoType type)
{
    if (type != ROCKET && type != PLASMA && type != REGULAR)
        return;

    if (m_currentAmmo[type] == m_maxAmmo)
        std::cout << "Clip full" << std::endl;
    else
        m_currentAmmo[type] += 1;
}

int Phaser::getCurrentAmmos() const
{
    return m_currentAmmo[m_type];
}
