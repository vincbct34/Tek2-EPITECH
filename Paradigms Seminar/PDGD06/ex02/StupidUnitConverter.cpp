/*
** EPITECH PROJECT, 2025
** Paradigms Seminar
** File description:
** Exercise 2
*/

#include <iomanip>
#include <iostream>
#include <string>
#include <sstream>

void convertToFahrenheit(double temperature) {
    double convertedTemperature = (temperature * 9 / 5) + 32;

    std::cout << std::setw(16) << std::right << std::fixed << std::setprecision(3) << convertedTemperature << std::setw(16) << "Fahrenheit" << std::endl;
}

void convertToCelsius(double temperature) {
    double convertedTemperature = (temperature - 32) * 5 / 9;

    std::cout << std::setw(16) << std::right << std::fixed << std::setprecision(3) << convertedTemperature << std::setw(16) << "Celsius" << std::endl;
}

int main(void) {
    std::string scale;
    std::string input;
    double temperature;

    while (std::getline(std::cin, input)) {
        std::istringstream iss(input);
        iss >> temperature >> scale;

        if (scale == "Celsius") {
            convertToFahrenheit(temperature);
        } else if (scale == "Fahrenheit") {
            convertToCelsius(temperature);
        } else {
            continue;
        }
    }

    return 0;
}
