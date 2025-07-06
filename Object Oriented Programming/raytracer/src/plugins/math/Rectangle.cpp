/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Rectangle
*/

#include "Rectangle.hpp"

namespace Math {
    Rectangle3D::Rectangle3D(const Math::Point3D& origin, const Math::Vector3D& bottom_side, const Math::Vector3D& left_side)
        : origin(origin), bottom_side(bottom_side), left_side(left_side) {}

    Math::Point3D Rectangle3D::pointAt(double u, double v) const {
        return Math::Point3D(
            origin.getX() + u * bottom_side.getX() + v * left_side.getX(),
            origin.getY() + u * bottom_side.getY() + v * left_side.getY(),
            origin.getZ() + u * bottom_side.getZ() + v * left_side.getZ()
        );
    }
}