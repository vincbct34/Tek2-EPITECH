/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Point
*/

#pragma once

namespace Math {
    class Point3D {
        public:
            Point3D(float x, float y, float z) : x(x), y(y), z(z) {}
            ~Point3D() = default;

            float getX() const { return x; }
            float getY() const { return y; }
            float getZ() const { return z; }

            Point3D operator+(const Point3D &other) const { return Point3D(x + other.x, y + other.y, z + other.z); }
            Point3D operator+=(const Point3D &other) { x += other.x; y += other.y; z += other.z; return *this; }

        private:
            float x;
            float y;
            float z;
    };
}