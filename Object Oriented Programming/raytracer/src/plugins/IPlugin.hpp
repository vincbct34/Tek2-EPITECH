/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** IPlugin
*/

#pragma once

enum class PluginType {
    CAMERA,
    LIGHT,
    PRIMITIVE,
    RENDERER,
};

class IPlugin {
    public:
        IPlugin() = default;
        virtual ~IPlugin() = default;

        virtual PluginType getType() const = 0;
};
