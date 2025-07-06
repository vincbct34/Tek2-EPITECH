/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** SceneLoader
*/

#pragma once

#include "PluginLoader.hpp"
#include "SceneLoader.hpp"
#include "Exceptions.hpp"
#include "IRenderer.hpp"
#include "ICamera.hpp"
#include "IPlugin.hpp"
#include "ILight.hpp"

#include <libconfig.h++>

#include <iostream>
#include <vector>

using namespace libconfig;

class SceneLoader {
    public:
        SceneLoader() = default;
        ~SceneLoader() = default;

        bool checkCfgError(const std::string &filename);
        void loadCamera(IPlugin *camera);
        void loadLights(std::vector<IPlugin *> lights);
        void loadRender(IPlugin *plugin);
        std::vector<IPlugin *> loadPrimitives(PluginLoader &pluginLoader);

        void fillSphere(IPlugin *sphere, Setting &sphereSetting);
        void fillPlane(IPlugin *plane, Setting &planeSetting);
        void fillCylinder(IPlugin *cylinder, Setting &cylinderSetting);
        void fillCone(IPlugin *cone, Setting &coneSetting);
        void fillCube(IPlugin *cube, Setting &cubeSetting);

        template <typename T>
        T *castPlugin(IPlugin *plugin) {
            return dynamic_cast<T *>(plugin);
        }

    private:
        Config cfg;
};
