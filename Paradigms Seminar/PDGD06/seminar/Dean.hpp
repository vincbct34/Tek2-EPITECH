/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 5
*/

#pragma once

#include "Student.hpp"

class Dean {
    private:
        std::string name;
        bool isBusy = false;

    public:
        Dean(std::string name);
        ~Dean();

        void teachStudent(Student *student, std::string subject);
        void timeCheck();
        std::string getName();
};
