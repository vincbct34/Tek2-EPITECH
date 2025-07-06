/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** IPrimitive
*/

#pragma once

#include "HitRecord.hpp"
#include "IPlugin.hpp"
#include "Vector.hpp"
#include "Color.hpp"
#include "Point.hpp"
#include "Ray.hpp"

#include <vector>

enum class PrimitiveType {
    SPHERE,
    PLANE,
    CUBE,
    CYLINDER,
    CONE,
    DONUT,
};

class IPrimitive : public IPlugin {
    public:
        IPrimitive() = default;
        virtual ~IPrimitive() = default;

        bool virtual hits(const RayTracer::Ray &ray) const = 0;
        void virtual translate(const Math::Vector3D &translation) = 0;
        void virtual rotate(float angle, const Math::Vector3D &axis) = 0;
        virtual bool intersect(const RayTracer::Ray &ray, RayTracer::HitRecord &rec) const = 0;

        // Getters
        virtual const Math::Point3D &getCenter() const = 0;
        virtual float getRadius() const = 0;
        virtual const Math::Color &getColor() const = 0;
        virtual Math::Point3D getPosition() const = 0;
        virtual PrimitiveType getPrimitiveType() const = 0;
        virtual Math::Vector3D getNormal() const = 0;
        virtual float getHeight() const = 0;



        // Setters
        virtual void setPosition(const Math::Point3D &position) = 0;
        virtual void setRadius(float radius) = 0;
        virtual void setColor(Math::Color color) = 0;
        virtual void setNormal(const Math::Vector3D &normal) = 0;
        virtual void setHeight(float height) = 0;

        // Get the type of the plugin
        PluginType getType() const override {
            return PluginType::PRIMITIVE;
        }
};