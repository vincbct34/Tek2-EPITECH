/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** ILight
*/

#pragma once

#include "IPlugin.hpp"
#include "Vector.hpp"
#include "Point.hpp"

enum class LightType {
    AMBIENT,
    DIRECTIONAL
};

class ILight : public IPlugin {
    public:
    ILight() = default;
    virtual ~ILight() = default;

    virtual LightType getLightType() const = 0;

    virtual void setIntensity(float intensity) = 0;
    virtual float getIntensity() const = 0;
    virtual void applyLight(float &r, float &g, float &b) const = 0;

    virtual void setDirection(float x, float y, float z) = 0;
    virtual Math::Vector3D getDirection() const = 0;
};
