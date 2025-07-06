/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Rectangle
*/

#pragma once

#include "Vector.hpp"
#include "Point.hpp"

namespace Math {
    class Rectangle3D {
    public:
        Rectangle3D(const Math::Point3D& origin, const Math::Vector3D& bottom_side, const Math::Vector3D& left_side);

        Math::Point3D origin; // Bottom-left corner of the rectangle
        Math::Vector3D bottom_side;
        Math::Vector3D left_side;

        Math::Point3D pointAt(double u, double v) const;
    };
}