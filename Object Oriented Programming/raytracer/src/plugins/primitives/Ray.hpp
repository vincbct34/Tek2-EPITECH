/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Ray
*/

#pragma once

#include "Vector.hpp"
#include "Point.hpp"

namespace RayTracer {
    class Ray {
        public:
        Ray(const Math::Point3D &origin, const Math::Vector3D &direction);

        //Getters

        const Math::Point3D &getOrigin() const {
            return origin;
        }
        const Math::Vector3D &getDirection() const {
            return direction;
        }

        private:
        Math::Point3D origin;
        Math::Vector3D direction;
    };
}