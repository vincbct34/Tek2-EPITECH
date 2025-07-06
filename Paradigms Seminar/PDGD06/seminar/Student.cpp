/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 3
*/

#include "Student.hpp"

#include <iostream>

Student::Student(std::string studentName) : name(studentName)
{
    std::cout << "Student " << name << ": I'm ready to learn C++." << std::endl;
}

Student::~Student()
{
    std::cout << "Student " << name << ": Wow, much learning today, very smart, such C++." << std::endl;
}

bool Student::learn(std::string text)
{
    size_t pos = 0;

    if (energy >= 42) {
        energy -= 42;
        std::cout << "Student " << name << ": " << text << std::endl;
    } else {
        while ((pos = text.find("C++", pos)) != std::string::npos) {
            text.replace(pos, 3, "shit");
            pos += 4;
        }
        std::cout << "Student " << name << ": " << text << std::endl;
        return false;
    }

    return true;
}

void Student::drink(std::string drink)
{
    if (drink == "Red Bull") {
        std::cout << "Student " << name << ": Red Bull gives you wings!" << std::endl;
        energy += 32;
    } else if (drink == "Monster") {
        std::cout << "Student " << name << ": Unleash The Beast!" << std::endl;
        energy += 64;
    } else {
        std::cout << "Student " << name << ": ah, yes... enslaved moisture." << std::endl;
        energy += 1;
    }

    if (energy > 100)
        energy = 100;
}

std::string Student::getName()
{
    return name;
}

int Student::getEnergy()
{
    return energy;
}
