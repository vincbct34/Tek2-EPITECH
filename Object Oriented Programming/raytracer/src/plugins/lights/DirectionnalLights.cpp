/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** DirectionnalLights
*/

#include "DirectionnalLights.hpp"
#include "Rectangle.hpp"
#include "Vector.hpp"
#include "Point.hpp"

DirectionnalLights::DirectionnalLights(float intensity, Math::Vector3D direction)
    : intensity(intensity), position(0.0f, 0.0f, 0.0f), direction(direction) {}

void DirectionnalLights::setIntensity(float intensity) {
    this->intensity = intensity;
}

float DirectionnalLights::getIntensity() const {
    return intensity;
}

void DirectionnalLights::setDirection(float x, float y, float z) {
    direction = Math::Vector3D(x, y, z);
}

Math::Vector3D DirectionnalLights::getDirection() const {
    return direction;
}

void DirectionnalLights::applyLight(float &r, float &g, float &b) const {
    // Exemple d'application de la lumière directionnelle
    float factor = intensity; // Ajustez selon vos besoins
    r *= factor;
    g *= factor;
    b *= factor;
}

extern "C" IPlugin *createDirectionnalLights() {
    return new DirectionnalLights();
}
