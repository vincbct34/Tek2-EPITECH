/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Date and Time Module
*/

#include "../../include/DateTimeInfoModule.hpp"

#include <ncurses.h>
#include <ctime>
#include <sstream>

DateTimeModule::DateTimeModule()
    : IModule(IModule::Mode::Performance, IModule::DataType::Text, "Date and Time")
{
    update();
}

void DateTimeModule::update()
{
    std::time_t now = std::time(nullptr);
    std::tm* localTime = std::localtime(&now);
    
    std::ostringstream oss;
    oss << (1900 + localTime->tm_year) << "-" 
        << (1 + localTime->tm_mon) << "-" 
        << localTime->tm_mday << " "
        << localTime->tm_hour << ":" 
        << localTime->tm_min << ":" 
        << localTime->tm_sec;
    
    dateTimeStr = oss.str();
}


std::string DateTimeModule::getData() const
{
    return dateTimeStr;
}
