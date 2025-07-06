/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** AmbientLight
*/

#include "AmbientLight.hpp"

AmbientLight::AmbientLight(float intensity)
    : intensity(intensity) {}

void AmbientLight::setIntensity(float intensity) {
    this->intensity = intensity;
}

float AmbientLight::getIntensity() const {
    return intensity;
}

void AmbientLight::applyLight(float &r, float &g, float &b) const {
    r *= intensity;
    g *= intensity;
    b *= intensity;
}

extern "C" IPlugin *createAmbientLight() {
    return new AmbientLight();
}
