/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Cube
*/

#pragma once

#include "IPrimitive.hpp"
#include "Vector.hpp"
#include "Point.hpp"
#include "Ray.hpp"

#include <algorithm>
#include <cmath>

namespace RayTracer {
    class Cube : public IPrimitive {
    public:
        Cube();
        ~Cube() = default;

        bool hits(const Ray &ray) const override;
        bool intersect(const Ray &ray, HitRecord &rec) const override;
        void translate(const Math::Vector3D &translation) override;
        void rotate(float angle, const Math::Vector3D &axis) override;

        // Getters
        const Math::Point3D &getCenter() const override {
            return center;
        }
        float getRadius() const override {
            return size / 2.0f; // Demi-taille du cube
        }
        const Math::Color &getColor() const override {
            return color;
        }

        // Get the type of the plugin
        PluginType getType() const override {
            return PluginType::PRIMITIVE;
        }

        // Get the primitive type
        PrimitiveType getPrimitiveType() const override {
            return PrimitiveType::CUBE;
        }

        Math::Point3D getPosition() const override {
            return center;
        }
        void setPosition(const Math::Point3D &position) override {
            center = position;
        }
        void setRadius(float newSize) override {
            size = newSize * 2.0f; // Le rayon est la moitié de la taille
        }
        void setNormal(const Math::Vector3D &newNormal) override {
            // Pas applicable directement pour un cube
            (void)newNormal;
        }
        Math::Vector3D getNormal() const override {
            // Un cube n'a pas une seule normale
            return Math::Vector3D(0, 0, 0);
        }
        void setColor(Math::Color newColor) override {
            color = newColor;
        }
        
        // Spécifique au cube
        void setSize(float newSize) {
            size = newSize;
        }
        float getSize() const {
            return size;
        }

        // Set height
        void setHeight(float newHeight) override {
            // Pas applicable directement pour un cube
            (void)newHeight;
        }
        float getHeight() const override {
            // Pas applicable directement pour un cube
            return size;
        }

    private:
        Math::Point3D center; // Centre du cube
        float size;           // Taille du cube (longueur d'une arête)
        Math::Color color;    // Couleur du cube
        
        // Directions des axes du cube (pour supporter la rotation)
        Math::Vector3D xAxis;
        Math::Vector3D yAxis;
        Math::Vector3D zAxis;
    };
} 