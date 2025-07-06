/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 6
*/

#pragma once

#include "Assistant.hpp"
#include "Dean.hpp"

#include <vector>

class Seminar {
    private:
        Dean* dean;
        std::vector<Assistant*> assistants;
        std::vector<Student*> students;
        const size_t maxAssistants;
        const size_t maxStudents;

    public:
        Seminar();
        ~Seminar();

        void addDean(Dean* newDean);
        void addAssistant(Assistant* newAssistant);
        void addStudent(Student* newStudent);
        void run();
};
