/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** BasicRenderer
*/

#include "IRenderer.hpp"
#include "ICamera.hpp"
#include "Sphere.hpp"
#include "Ray.hpp"

#include <iostream>
#include <fstream>
#include <cmath>

#pragma once

class BasicRenderer : public IRenderer {
    public:
        BasicRenderer();
        ~BasicRenderer() = default;

        // Get the type of the plugin
        PluginType getType() const override {
            return PluginType::RENDERER;
        }

        // Set the renderer type
        void setRendererType(RendererType type) override {
            this->type = type;
        }

        // Get the renderer type
        RendererType getRendererType() const override {
            return this->type;
        }

        // Function to render the scene
        void renderScene() override;

        // Function to set the camera
        void setCamera(RayTracer::ICamera *camera) override;

        // Function to set the lights
        void setLights(std::vector<ILight *> &lights) override;

        // Function to set the primitives
        void setPrimitives(std::vector<IPrimitive *> &primitives) override;

    private:
        RendererType type;
        RayTracer::ICamera *camera;
        std::vector<ILight *> lights;
        std::vector<IPrimitive *> primitives;
};