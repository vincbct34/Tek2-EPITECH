/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 6
*/

#include "Seminar.hpp"

#include <iostream>
#include <algorithm>

Seminar::Seminar()
    : dean(nullptr), maxAssistants(2), maxStudents(5)
{
}

Seminar::~Seminar()
{
}

void Seminar::addDean(Dean* newDean)
{
    if (dean)
        std::cout << "Seminar: There can only be one Dean of Studies." << std::endl;
    else {
        dean = newDean;
        std::cout << "Seminar: Dean " << newDean->getName() << " is here." << std::endl;
    }
}

void Seminar::addAssistant(Assistant* newAssistant)
{
    if (std::find(assistants.begin(), assistants.end(), newAssistant) != assistants.end())
        std::cout << "Seminar: Assistant " << newAssistant->getID() << " is already registered." << std::endl;
    else if (assistants.size() >= maxAssistants)
        std::cout << "Seminar: There is only room for two Teaching Assistants." << std::endl;
    else {
        assistants.push_back(newAssistant);
        std::cout << "Seminar: Assistant " << newAssistant->getID() << " joined the pedagogical team." << std::endl;
    }
}

void Seminar::addStudent(Student* newStudent)
{
    if (std::find(students.begin(), students.end(), newStudent) != students.end())
        std::cout << "Seminar: Student " << newStudent->getName() << " is already registered." << std::endl;
    else if (students.size() >= maxStudents)
        std::cout << "Seminar: There is only room for five Students." << std::endl;
    else {
        students.push_back(newStudent);
        std::cout << "Seminar: Student " << newStudent->getName() << " joined the seminar." << std::endl;
    }
}

void Seminar::run()
{
    size_t assistantIndex = 0;

    if (!dean || assistants.empty() || students.empty()) {
        std::cout << "Seminar: A C++ seminar needs at least one Dean of Studies, one Assistant and one Student." << std::endl;
        return;
    }

    std::cout << "Seminar: Begining 6th day of seminar." << std::endl;
    std::cout << "Dean of Studies: " << dean->getName() << std::endl;

    std::cout << "Teaching assistants: ";
    for (size_t i = 0; i < assistants.size(); ++i) {
        std::cout << assistants[i]->getID();
        if (i < assistants.size() - 1) std::cout << ", ";
    }
    std::cout << std::endl;

    std::cout << "Students: ";
    for (size_t i = 0; i < students.size(); ++i) {
        std::cout << students[i]->getName();
        if (i < students.size() - 1) std::cout << ", ";
    }
    std::cout << std::endl;

    dean->timeCheck();
    for (auto assistant : assistants)
        assistant->timeCheck();

    for (int i = 0; i < 5; ++i) {
        for (auto student : students) {
            dean->teachStudent(student, "I'm learning C++!");
            assistants[assistantIndex]->helpStudent(student);
            assistantIndex = (assistantIndex + 1) % assistants.size();
        }
    }

    dean->timeCheck();

    for (auto assistant : assistants)
        assistant->timeCheck();
}
