/*
** EPITECH PROJECT, 2025
** RayTracer [WSL: Ubuntu-24.04]
** File description:
** Cylinder
*/

#pragma once

#include "IPrimitive.hpp"
#include "Vector.hpp"
#include "Color.hpp"
#include "Point.hpp"
#include "Ray.hpp"

#include <iostream>
#include <cmath>

namespace RayTracer {
    class Cylinder : public IPrimitive {
    public:
        Cylinder();
        ~Cylinder() = default;

        bool hits(const Ray &ray) const override;
        bool intersect(const Ray &ray, HitRecord &rec) const override;
        void translate(const Math::Vector3D &translation) override;
        void rotate(float angle, const Math::Vector3D &axis) override;

        const Math::Point3D &getCenter() const override { return base; }
        float getRadius() const override { return radius; }
        const Math::Color &getColor() const override { return color; }

        PrimitiveType getPrimitiveType() const override { return PrimitiveType::CYLINDER; }
        Math::Point3D getPosition() const override { return base; }
        void setPosition(const Math::Point3D &position) override { base = position; }
        void setRadius(float newRadius) override { radius = newRadius; }
        void setNormal(const Math::Vector3D &newNormal) override { axis = newNormal; }
        Math::Vector3D getNormal() const override { return axis; }
        void setColor(Math::Color newColor) override { color = newColor; }

        float getHeight() const override { return height; }
        void setHeight(float newHeight) override { height = newHeight; }

    private:
        Math::Point3D base;
        Math::Vector3D axis;
        float radius;
        float height;
        Math::Color color;
    };
}
