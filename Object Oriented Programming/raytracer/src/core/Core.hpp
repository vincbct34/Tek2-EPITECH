/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Core
*/

#pragma once

#include "PluginLoader.hpp"
#include "SceneLoader.hpp"
#include "IPrimitive.hpp"
#include "Exceptions.hpp"
#include "IRenderer.hpp"
#include "ICamera.hpp"
#include "IPlugin.hpp"
#include "ILight.hpp"

#include <SFML/Graphics.hpp>
#include <SFML/Window.hpp>
#include <iostream>
#include <fstream>
#include <cstring>
#include <dlfcn.h>
#include <string>
#include <vector>

class Core {
    public:
        Core() = default;

        bool isFileValid(const std::string &filename);

        std::vector<IPlugin*> getPlugins() const {
            return plugins;
        }

        void addPlugin(IPlugin *plugin) {
            plugins.push_back(plugin);
        }

        void convertVectorPluginToVectorLight(std::vector<IPlugin*> &plugins, std::vector<ILight*> &lights);
        void convertVectorPluginToVectorPrimitive(std::vector<IPlugin*> &plugins, std::vector<IPrimitive*> &primitives);

    private:
        std::vector<IPlugin*> plugins;
};
