/*
** EPITECH PROJECT, 2025
** Interstonar [WSL: Ubuntu-24.04]
** File description:
** Vector3
*/

#pragma once

#include <iostream>
#include <cmath>

class Vector3 {
public:
    double x, y, z;

    Vector3();
    Vector3(const Vector3& other);
    Vector3(double x, double y, double z);

    double norm() const;
    double length() const;
    Vector3 abs() const;
    Vector3 normalize() const;

    Vector3 operator+(const Vector3& other) const;
    Vector3 operator-(const Vector3& other) const;
    Vector3& operator=(const Vector3& other);
    Vector3 operator*(double scalar) const;
    Vector3 operator/(double scalar) const;
};
