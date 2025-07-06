/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** DirectionnalLights
*/

#pragma once

#include "ILight.hpp"

class DirectionnalLights : public ILight {
    public:
        DirectionnalLights(float intensity = 1.0f, Math::Vector3D direction = Math::Vector3D(0, 0, -1));
        ~DirectionnalLights() = default;

        // Get the type of the plugin
        PluginType getType() const override {
            return PluginType::LIGHT;
        }

        // Get the light type
        LightType getLightType() const override {
            return LightType::DIRECTIONAL;
        }

        // Setters and Getters for light intensity
        void setIntensity(float intensity) override;
        float getIntensity() const override;

        // Setters and Getters for light direction
        void setDirection(float x, float y, float z) override;
        Math::Vector3D getDirection() const override;

        // function to Apply the light to a color
        void applyLight(float &r, float &g, float &b) const override;
    private:
        float intensity;
        Math::Point3D position;
        Math::Vector3D direction;
};