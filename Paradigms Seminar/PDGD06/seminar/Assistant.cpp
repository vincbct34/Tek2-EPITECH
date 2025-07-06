/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 4
*/

#include "Assistant.hpp"

#include <iostream>
#include <fstream>

Assistant::Assistant(int identificationNumber) : ID(identificationNumber)
{
    std::cout << "Assistant " << ID << ": 'morning everyone *sip coffee*" << std::endl;
}

Assistant::~Assistant()
{
    std::cout << "Assistant " << ID << ": see you tomorrow at 9.00 *sip coffee*" << std::endl;
}

void Assistant::giveDrink(Student *student, std::string drinkName)
{
    std::cout << "Assistant " << ID << ": drink this, " << student->getName() << " *sip coffee*" << std::endl;
    student->drink(drinkName);
}

std::string Assistant::readDrink(std::string studentName)
{
    std::ifstream file(studentName + ".drink");
    std::string drink;

    if (!file.is_open())
        return "";

    std::getline(file, drink);

    file.close();

    std::remove((studentName + ".drink").c_str());
    std::cout << "Assistant " << ID << ": " << studentName << " needs a " << drink << " *sip coffee*" << std::endl;
    
    return drink;
}

void Assistant::helpStudent(Student *student)
{
    std::string drink = readDrink(student->getName());

    if (drink != "")
        giveDrink(student, drink);
    else
        std::cout << "Assistant " << ID << ": " << student->getName() << " seems fine *sip coffee*" << std::endl;
}

void Assistant::timeCheck()
{
    if (isBusy) {
        std::cout << "Assistant " << ID << ": Enough teaching for today *sip coffee*" << std::endl;
        isBusy = false;
    } else {
        std::cout << "Assistant " << ID << ": Time to teach some serious business *sip coffee*" << std::endl;
        isBusy = true;
    }
}

int Assistant::getID()
{
    return ID;
}
