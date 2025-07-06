/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** ICamera
*/

#pragma once

#include "IPlugin.hpp"
#include "Point.hpp"
#include "Ray.hpp"

namespace RayTracer {
    class ICamera : public IPlugin {
    public:
        virtual ~ICamera() = default;

        //rotate the camera around the axis
        virtual void rotate(float angle, const Math::Vector3D &axis) = 0;
        virtual void translate(const Math::Vector3D &translation) = 0;
    
        virtual void setPosition(Math::Point3D position) = 0;
        virtual Math::Point3D getPosition() const = 0;
    
        virtual void setResolution(int width, int height) = 0;
        virtual void getResolution(int &width, int &height) const = 0;
    
        virtual void setFieldOfView(float fov) = 0;
        virtual float getFieldOfView() const = 0;

        virtual int getWidth() const = 0;
        virtual int getHeight() const = 0;

        virtual void updateScreen() = 0;
    
        virtual Ray rayAt(double u, double v) const = 0;

        virtual void setLookAt(const Math::Point3D &lookAt) = 0;
        virtual Math::Point3D getLookAt() const = 0;
    };
}