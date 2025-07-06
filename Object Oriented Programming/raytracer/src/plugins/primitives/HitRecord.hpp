/*
** EPITECH PROJECT, 2025
** RayTracer [WSL: Ubuntu-24.04]
** File description:
** HitRecord
*/

#pragma once

#include "Vector.hpp"
#include "Point.hpp"
#include "Color.hpp"

namespace RayTracer {
    struct HitRecord {
        Math::Point3D point = Math::Point3D(0,0,0);
        Math::Vector3D normal = Math::Vector3D(0,0,0);
        float t = 0;
        Math::Color color = Math::Color(0,0,0);
    };
}
