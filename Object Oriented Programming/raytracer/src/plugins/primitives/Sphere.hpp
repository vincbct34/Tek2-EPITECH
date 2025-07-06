/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Sphere
*/

#pragma once

#include "IPrimitive.hpp"
#include "Matrix.hpp"
#include "Point.hpp"
#include "Ray.hpp"

#include <cmath>

namespace RayTracer {
    class Sphere : public IPrimitive {
        public:
            Sphere();
            ~Sphere() = default;

            // Get the type of the plugin
            PluginType getType() const override {
                return PluginType::PRIMITIVE;
            }

            // Get the primitive type
            PrimitiveType getPrimitiveType() const override {
                return PrimitiveType::SPHERE;
            }
            // hits function
            bool hits(const Ray &ray) const override;
            bool intersect(const Ray &ray, RayTracer::HitRecord &rec) const override;
            void translate(const Math::Vector3D &translation) override;
            void rotate(float angle, const Math::Vector3D &axis) override;

            // Getters
            const Math::Point3D &getCenter() const override {
                return center;
            }
            float getRadius() const override {
                return radius;
            }

            // Get color
            const Math::Color &getColor() const override {
                return color;
            }

            void setPosition(const Math::Point3D &newCenter) override {
                center = newCenter;
            }
            Math::Point3D getPosition() const override {
                return center;
            }
            void setRadius(float newRadius) override {
                radius = newRadius;
            }
            void setNormal(const Math::Vector3D &newNormal) override  {
                (void)newNormal; // Not applicable for Sphere
            }
            Math::Vector3D getNormal() const override {
                return Math::Vector3D(0, 0, 0); // Not applicable for Sphere
            }
            void setColor(Math::Color newColor) override {
                color = newColor;
            }

            float getHeight() const override {
                return 0; // Not applicable for Sphere
            }
            void setHeight(float newHeight) override {
                (void)newHeight; // Not applicable for Sphere
            }

        private:
        Math::Point3D center;
        float radius;
        Math::Color color; // Color of the sphere
    };
}