/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 4
*/

#pragma once

#include "Student.hpp"

class Assistant {
    private:
        int ID;
        bool isBusy = false;

    public:
        Assistant(int ID);
        ~Assistant();

        void giveDrink(Student *student, std::string drink);
        std::string readDrink(std::string studentName);
        void helpStudent(Student *student);

        void timeCheck();
        int getID();
};
