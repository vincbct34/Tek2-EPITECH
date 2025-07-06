/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Camera
*/

#pragma once

#include "Rectangle.hpp"
#include "ICamera.hpp"
#include "Point.hpp"
#include "Ray.hpp"

#include <cmath>

namespace RayTracer {
    class Camera : public ICamera {
        public:
            Camera();
            Camera(const Math::Point3D& origin, const Math::Rectangle3D& screen, float fov);
            ~Camera() = default;

            // Get the type of the plugin
            PluginType getType() const override {
                return PluginType::CAMERA;
            }

            // Rotate the camera around the axis
            void rotate(float angle, const Math::Vector3D &axis) override;
            // Translate the camera
            void translate(const Math::Vector3D &translation) override;

            // Setters and Getters for camera position
            void setPosition(Math::Point3D position) override { origin = position; }
            Math::Point3D getPosition() const override;

            // Setters and Getters for camera resolution
            void setResolution(int width, int height) override;
            void getResolution(int &width, int &height) const override;

            // Setters and Getters for camera field of view
            void setFieldOfView(float fov) override;
            float getFieldOfView() const override;

            int getWidth() const override { return width; }
            int getHeight() const override { return height; }

            // Update the screen rectangle based on the field of view
            void updateScreen() override;

            Ray rayAt(double u, double v) const override;

            void setLookAt(const Math::Point3D &lookAt) override;
            Math::Point3D getLookAt() const override { return lookAt_; }

        private:
            Math::Point3D origin;
            Math::Point3D lookAt_;
            Math::Rectangle3D screen;
            float fieldOfView;
            int width;
            int height;
    };
}