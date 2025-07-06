/*
** EPITECH PROJECT, 2025
** Interstonar [WSL: Ubuntu-24.04]
** File description:
** Vector3
*/

#include "Vector3.hpp"

Vector3::Vector3() : x(0.0), y(0.0), z(0.0) {}

Vector3::Vector3(const Vector3& other) : x(other.x), y(other.y), z(other.z) {}

Vector3::Vector3(double x, double y, double z) : x(x), y(y), z(z) {}

double Vector3::norm() const {
    return std::sqrt(x * x + y * y + z * z);
}

double Vector3::length() const {
    return norm();
}

Vector3 Vector3::abs() const {
    return Vector3(std::abs(x), std::abs(y), std::abs(z));
}

Vector3 Vector3::normalize() const {
    const double epsilon = 1e-10; // Petite valeur pour gérer les erreurs d'arrondi
    double n = norm();

    if (n < epsilon) // Vérifie si la norme est proche de zéro
        return Vector3(0, 0, 0);

    return *this / n; // Divise chaque composante par la norme
}

Vector3 Vector3::operator+(const Vector3& other) const {
    return Vector3(x + other.x, y + other.y, z + other.z);
}

Vector3 Vector3::operator-(const Vector3& other) const {
    return Vector3(x - other.x, y - other.y, z - other.z);
}

Vector3& Vector3::operator=(const Vector3& other) {
    if (this != &other) {
        x = other.x;
        y = other.y;
        z = other.z;
    }
    return *this;
}

Vector3 Vector3::operator*(double scalar) const {
    return Vector3(x * scalar, y * scalar, z * scalar);
}

Vector3 Vector3::operator/(double scalar) const {
    return Vector3(x / scalar, y / scalar, z / scalar);
}
