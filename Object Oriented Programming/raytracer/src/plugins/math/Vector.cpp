/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Vector
*/

#include "Vector.hpp"
   
namespace Math {
    double Vector3D::dot(const Vector3D& other) const {
        return x * other.x + y * other.y + z * other.z;
    }
}