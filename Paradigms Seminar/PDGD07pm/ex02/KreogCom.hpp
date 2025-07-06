/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2 - Houston, we have a problem
*/

#pragma once

class KreogCom {
    public:
        KreogCom(int x, int y, int serial);
        ~KreogCom();

        void addCom(int x, int y, int serial);
        void removeCom();
        KreogCom *getCom();

        void ping() const;
        void locateSquad() const;

    private:
        const int m_serial;
        int m_x;
        int m_y;
        KreogCom *m_next;
        KreogCom *m_prev;
};
