/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** AmbientLight
*/

#pragma once

#include "ILight.hpp"

class AmbientLight : public ILight {
    public:
        AmbientLight(float intensity = 1.0f);
        ~AmbientLight() = default;

        // Get the type of the plugin
        PluginType getType() const override {
            return PluginType::LIGHT;
        }

        // Get the light type
        LightType getLightType() const override {
            return LightType::AMBIENT;
        }

        // Setters and Getters for light intensity
        void setIntensity(float intensity) override;
        float getIntensity() const override;

        // function to Apply the light to a color
        void applyLight(float &r, float &g, float &b) const override;

        void setDirection(float, float, float) override {} // No direction for ambient light
        Math::Vector3D getDirection() const override {return Math::Vector3D(0, 0, 0);} // No direction for ambient light
    private:
        float intensity;
};