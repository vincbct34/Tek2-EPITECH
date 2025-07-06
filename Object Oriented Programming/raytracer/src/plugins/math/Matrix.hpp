/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Matrix
*/

#pragma once

#include "Vector.hpp"
#include "Point.hpp"

#include <cmath>

namespace Math {
    class Matrix4x4 {
    public:
        Matrix4x4();
        ~Matrix4x4() = default;

        // Create identity matrix
        void identity();
        
        // Create rotation matrix around an axis
        void rotationMatrix(float angle, const Vector3D &axis);
        
        // Apply matrix to a point
        Point3D applyToPoint(const Point3D &point) const;
        
        // Apply matrix to a vector
        Vector3D applyToVector(const Vector3D &vector) const;

    private:
        float data[4][4];
    };
} 