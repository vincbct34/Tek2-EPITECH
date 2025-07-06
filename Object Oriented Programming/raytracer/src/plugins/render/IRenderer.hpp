/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** IRenderer
*/

#pragma once

#include "IPrimitive.hpp"
#include "IPlugin.hpp"
#include "ICamera.hpp"
#include "ILight.hpp"

#include <vector>

enum class RendererType {
    BASIC,
    SFML,
    FILE,
};

class IRenderer : public IPlugin {
    public:
        IRenderer() = default;
        virtual ~IRenderer() = default;

        // Set RendererType
        virtual void setRendererType(RendererType type) = 0;

        // Get the type of the renderer
        virtual RendererType getRendererType() const = 0;

        // Function to render the scene
        virtual void renderScene() = 0;

        // Function to set the camera
        virtual void setCamera(RayTracer::ICamera *camera) = 0;

        // Function to set the lights
        virtual void setLights(std::vector<ILight *> &lights) = 0;

        // Function to set the primitives
        virtual void setPrimitives(std::vector<IPrimitive *> &primitives) = 0;
};