/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Plane
*/

#pragma once

#include "IPrimitive.hpp"
#include "Vector.hpp"
#include "Point.hpp"
#include "Ray.hpp"

#include <cmath>

namespace RayTracer {
    class Plane : public IPrimitive {
    public:
        Plane();
        ~Plane() = default;

            bool hits(const RayTracer::Ray &ray) const override;
            void translate(const Math::Vector3D &translation) override;
            void rotate(float angle, const Math::Vector3D &axis) override;

            virtual bool intersect(const RayTracer::Ray &ray, RayTracer::HitRecord &rec) const override;

            // Getters
            const Math::Point3D &getCenter() const override {
                return point;
            }
            float getRadius() const override {
                return 0; // Planes do not have a radius
            }
            const Math::Color &getColor() const override {
                return color;
            }

            // Get the type of the plugin
            PluginType getType() const override {
                return PluginType::PRIMITIVE;
            }

            void setPosition(const Math::Point3D &newPoint) override {
            point = newPoint;
            }
            Math::Point3D getPosition() const override {
                    return point;
            }
            void setRadius(float newRadius) override {
                // Not applicable for Plane
                (void)newRadius;
            }
            void setNormal(const Math::Vector3D &newNormal) override {
                normal = newNormal;
            }

            Math::Vector3D getNormal() const override {
                return normal;
            }

            void setColor(Math::Color newColor) override {
                color = newColor;
            }

            // Get the primitive type
            PrimitiveType getPrimitiveType() const override{
                return PrimitiveType::PLANE;
            }

            // Set the height (not applicable for Plane)
            void setHeight(float newHeight) override {
                // Not applicable for Plane
                (void)newHeight;
            }
            float getHeight() const override {
                return 0; // Planes do not have a height
            }

        private:
            Math::Point3D point;
            Math::Vector3D normal;
            Math::Color color; // Color of the plane
    };
}

