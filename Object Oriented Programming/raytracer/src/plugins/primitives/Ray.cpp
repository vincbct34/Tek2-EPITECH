/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Ray
*/

#include "Ray.hpp"

RayTracer::Ray::Ray(const Math::Point3D &origin, const Math::Vector3D &direction)
    : origin(origin), direction(direction) {}