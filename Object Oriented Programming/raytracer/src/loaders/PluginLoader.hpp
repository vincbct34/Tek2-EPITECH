/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** PluginLoader
*/

#pragma once

#include "Exceptions.hpp"
#include "IPlugin.hpp"

#include <unordered_map>
#include <iostream>
#include <dlfcn.h>
#include <vector>
#include <string>

class PluginLoader {
    public:
        PluginLoader() = default;
        ~PluginLoader() = default;

        // Load the plugin from the given path
        void storeHandle(void *handle, const std::string &path);
        IPlugin *loadCameraPlugin(const std::string &path);
        std::vector<IPlugin *>loadlightsPlugin(const std::string &path);
        IPlugin *loadRenderPlugin(const std::string &path);

        void *loadSharedLibrary(const std::string &path, void *&handle);
        void *loadSymbol(void *handle, const std::string &symbolName);
        IPlugin *createPrimitive(const std::string &libPath, const std::string &symbolName);
        static void *&chooseHandleByPath(const std::string &path);

        // Unload the plugin
        void unloadCamera(IPlugin *camera);
        void unloadRender(IPlugin *plugin);
        void unloadLights(std::vector<IPlugin *> lights);
        void unloadPrimitives(std::vector<IPlugin *> primitive);

        // Store the loaded plugins
        IPlugin *camera;
        IPlugin *render;
        std::vector<IPlugin *> lights;
        std::vector<IPlugin *> primitive;

        // Handle for the loaded plugin
        // This is a pointer to the shared library
        static void *planeHandle;
        static void *sphereHandle;
        static void *cylinderHandle;
        static void *coneHandle;
        static void *cubeHandle;
        static void *cameraHandle;
        static void *lightHandle;
        static void *renderHandle;

        // Store the loaded plugin handles
        static void *&getInstance(const std::string &path);

        private:
            static std::unordered_map<std::string, void **> _factory;
            static std::unordered_map<std::string, void **> _sceneFactory;
};
