/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Cone
*/

#pragma once

#include "IPrimitive.hpp"
#include "Vector.hpp"
#include "Point.hpp"
#include "Color.hpp"
#include "Ray.hpp"

#include <iostream>
#include <cmath>

namespace RayTracer {
    class Cone : public IPrimitive {
    public:
        // Constructeur : base est le sommet du cône, axis est la direction de l'axe, 
        // radius est le rayon de la base, height est la hauteur
        Cone();
        ~Cone() = default;

        // Interface IPrimitive
        bool hits(const Ray &ray) const override;
        bool intersect(const Ray &ray, HitRecord &rec) const override;
        void translate(const Math::Vector3D &translation) override;
        void rotate(float angle, const Math::Vector3D &axis) override;

        // Getters
        const Math::Point3D &getCenter() const override { return vertex; }
        float getRadius() const override { return radius; }
        const Math::Color &getColor() const override { return color; }

        // Get the type of the plugin
        PluginType getType() const override {
            return PluginType::PRIMITIVE;
        }

        // Get the primitive type
        PrimitiveType getPrimitiveType() const override{
            return PrimitiveType::CONE;
        }

        Math::Point3D getPosition() const override { return vertex; }
        void setPosition(const Math::Point3D &position) override { vertex = position; }
        void setRadius(float newRadius) override { radius = newRadius; }
        void setNormal(const Math::Vector3D &newNormal) override { axis = newNormal; }
        Math::Vector3D getNormal() const override { return axis; }
        void setColor(Math::Color newColor) override { color = newColor; }

        float getHeight() const override { return height; }
        void setHeight(float newHeight) override { height = newHeight; }

    private:
        Math::Point3D vertex;    // Sommet du cône
        Math::Vector3D axis;     // Direction de l'axe
        float radius;            // Rayon de la base
        float height;            // Hauteur du cône
        Math::Color color;       // Couleur du cône
    };
} 