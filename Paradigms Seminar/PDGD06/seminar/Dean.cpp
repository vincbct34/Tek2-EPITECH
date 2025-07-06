/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 5
*/

#include "Dean.hpp"
#include "Student.hpp"

#include <cstdio>
#include <iostream>
#include <fstream>
#include <vector>

Dean::Dean(std::string deanName) : name(deanName)
{
    std::cout << "Dean " << name << ": I'm Dean " << name << "! How do you do, fellow kids?" << std::endl;
}

Dean::~Dean()
{
    std::cout << "Dean " << name << ": Time to go home." << std::endl;
}

void Dean::teachStudent(Student *student, std::string subject)
{
    if (!student->learn(subject)) {
        std::cout << "Dean " << name << ": All work and no play makes " << student->getName() << " a dull student." << std::endl;
        std::vector<std::string> drinks = {"Cristaline", "Monster", "Evian", "Red Bull", "Sierra Springs"};
        std::string drink = drinks[rand() % 5];

        std::ofstream report(student->getName() + ".drink");
        if (report.is_open()) {
            report << drink;
            report.close();
        } else {
            std::cerr << "Unable to open file for writing report." << std::endl;
        }
    }
}

void Dean::timeCheck()
{
    if (isBusy) {
        std::cout << "Dean " << name << ": Don't forget to close the windows when you leave." << std::endl;
        isBusy = false;
    } else {
        std::cout << "Dean " << name << ": Where is everyone?" << std::endl;
        isBusy = true;
    }
}

std::string Dean::getName()
{
    return name;
}
