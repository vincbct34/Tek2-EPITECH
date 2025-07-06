/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 4 - Operators
*/

#include "Picture.hpp"

#include <fstream>
#include <sstream>

Picture::Picture(const std::string &file) {
    if (file.empty())
        data = "";
    else
        getPictureFromFile(file);
}

Picture::Picture()
    : data("")
{
}

Picture::~Picture()
{
}

bool Picture::getPictureFromFile(const std::string &fileName) {
    std::ifstream file(fileName);
    std::stringstream buffer;

    if (!file) {
        data = "ERROR";
        return false;
    }

    buffer << file.rdbuf();
    data = buffer.str();
    return true;
}

Picture &Picture::operator=(const Picture &other) {
    if (this != &other) {
        data = other.data;
    }
    return *this;
}
