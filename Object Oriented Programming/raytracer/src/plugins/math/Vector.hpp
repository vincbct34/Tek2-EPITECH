/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Vector
*/

#pragma once

namespace Math {
    class Vector3D {
        public:
            Vector3D(float x, float y, float z) : x(x), y(y), z(z) {}
            ~Vector3D() = default;

            float getX() const { return x; }
            float getY() const { return y; }
            float getZ() const { return z; }

            Vector3D operator+(const Vector3D &other) const { return Vector3D(x + other.x, y + other.y, z + other.z); }
            Vector3D operator-(const Vector3D &other) const { return Vector3D(x - other.x, y - other.y, z - other.z); }

            double dot(const Vector3D& other) const;
        private:
            float x;
            float y;
            float z;
    };
}