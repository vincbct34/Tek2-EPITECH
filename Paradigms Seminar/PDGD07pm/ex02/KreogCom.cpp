/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - Houston, we have a problem
*/

#include "KreogCom.hpp"

#include <iostream>

KreogCom::KreogCom(int x, int y, int serial)
    : m_serial(serial), m_x(x), m_y(y), m_next(nullptr), m_prev(nullptr)
{
    std::cout << "KreogCom " << m_serial << " initialized" << std::endl;
}

KreogCom::~KreogCom(void)
{
    KreogCom *tmp = this->m_next;

    if (this->m_prev) {
        this->m_prev->m_next = tmp;
    }
    if (this->m_next) {
        tmp->m_prev = this->m_prev;
    }
    std::cout << "KreogCom " << this->m_serial << " shutting down" << std::endl;
}

void KreogCom::addCom(int x, int y, int serial)
{
    KreogCom *newCom = new KreogCom(x, y, serial);

    newCom->m_prev = this;
    newCom->m_next = this->m_next;
    this->m_next = newCom;
    if (newCom->m_next) {
        newCom->m_next->m_prev = newCom;
    }
}

KreogCom *KreogCom::getCom()
{
    return m_next;
}

void KreogCom::removeCom(void)
{
    if (!this->m_next)
        return;
    delete this->m_next;
}

void KreogCom::ping() const
{
    std::cout << "KreogCom " << m_serial << " currently at " << m_x << " " << m_y << std::endl;
}

void KreogCom::locateSquad() const
{
    KreogCom *temp = this->m_next;

    if (temp != nullptr) {
        while (temp != nullptr) {
            temp->ping();
            temp = temp->m_next;
        }
    }
    this->ping();
}
