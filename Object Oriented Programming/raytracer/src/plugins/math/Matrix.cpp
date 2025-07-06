/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Matrix
*/

#include "Matrix.hpp"

Math::Matrix4x4::Matrix4x4() {
    identity();
}

void Math::Matrix4x4::identity() {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            data[i][j] = (i == j) ? 1.0f : 0.0f;
        }
    }
}

void Math::Matrix4x4::rotationMatrix(float angle, const Math::Vector3D &axis) {
    // Convert angle to radians
    float radians = angle * M_PI / 180.0f;
    float c = cos(radians);
    float s = sin(radians);
    float t = 1.0f - c;
    
    // Normalize the axis
    float magnitude = sqrt(axis.getX() * axis.getX() + 
                          axis.getY() * axis.getY() + 
                          axis.getZ() * axis.getZ());
    float x = axis.getX() / magnitude;
    float y = axis.getY() / magnitude;
    float z = axis.getZ() / magnitude;
    
    // Create the rotation matrix
    data[0][0] = t * x * x + c;
    data[0][1] = t * x * y - s * z;
    data[0][2] = t * x * z + s * y;
    data[0][3] = 0.0f;
    
    data[1][0] = t * x * y + s * z;
    data[1][1] = t * y * y + c;
    data[1][2] = t * y * z - s * x;
    data[1][3] = 0.0f;
    
    data[2][0] = t * x * z - s * y;
    data[2][1] = t * y * z + s * x;
    data[2][2] = t * z * z + c;
    data[2][3] = 0.0f;
    
    data[3][0] = 0.0f;
    data[3][1] = 0.0f;
    data[3][2] = 0.0f;
    data[3][3] = 1.0f;
}

Math::Point3D Math::Matrix4x4::applyToPoint(const Math::Point3D &point) const {
    float x = point.getX();
    float y = point.getY();
    float z = point.getZ();
    
    float newX = data[0][0] * x + data[0][1] * y + data[0][2] * z + data[0][3];
    float newY = data[1][0] * x + data[1][1] * y + data[1][2] * z + data[1][3];
    float newZ = data[2][0] * x + data[2][1] * y + data[2][2] * z + data[2][3];
    
    return Math::Point3D(newX, newY, newZ);
}

Math::Vector3D Math::Matrix4x4::applyToVector(const Math::Vector3D &vector) const {
    float x = vector.getX();
    float y = vector.getY();
    float z = vector.getZ();
    
    float newX = data[0][0] * x + data[0][1] * y + data[0][2] * z;
    float newY = data[1][0] * x + data[1][1] * y + data[1][2] * z;
    float newZ = data[2][0] * x + data[2][1] * y + data[2][2] * z;
    
    return Math::Vector3D(newX, newY, newZ);
} 