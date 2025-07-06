/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** PluginLoader
*/

#include "PluginLoader.hpp"

void *PluginLoader::planeHandle = nullptr;
void *PluginLoader::sphereHandle = nullptr;
void *PluginLoader::cylinderHandle = nullptr;
void *PluginLoader::coneHandle = nullptr;
void *PluginLoader::cubeHandle = nullptr;
void *PluginLoader::cameraHandle = nullptr;
void *PluginLoader::lightHandle = nullptr;
void *PluginLoader::renderHandle = nullptr;

std::unordered_map<std::string, void **> PluginLoader::_sceneFactory = {
    {"./plugins/camera.so", &PluginLoader::cameraHandle},
    {"./plugins/light.so", &PluginLoader::lightHandle},
    {"./plugins/render.so", &PluginLoader::renderHandle},
};

void PluginLoader::storeHandle(void *handle, const std::string &path) {
    auto it = _sceneFactory.find(path);

    if (it != _sceneFactory.end()) {
        *(it->second) = handle;
    } else {
        std::cerr << "Error: Unknown plugin path " << path << std::endl;
    }
}

IPlugin *PluginLoader::loadCameraPlugin(const std::string &path) {
    void *handle = dlopen(path.c_str(), RTLD_LAZY);
    
    if (!handle) {
        std::cerr << "Error loading plugin: " << dlerror() << std::endl;
        return nullptr;
    }
    dlerror();

    typedef IPlugin *(*create_t)();
    create_t createCameraPlugin = (create_t)dlsym(handle, "createCameraPlugin");
    const char *dlsym_error = dlerror();
    if (dlsym_error) {
        std::cerr << "Error loading symbol: " << dlsym_error << std::endl;
        dlclose(handle);
        return nullptr;
    }

    storeHandle(handle, path);

    IPlugin *plugin = createCameraPlugin();
    return plugin;
}

std::vector<IPlugin *> PluginLoader::loadlightsPlugin(const std::string &path) {
    void *handle = nullptr;

    if (!lightHandle) {
        handle = dlopen(path.c_str(), RTLD_LAZY);
        if (!handle) {
            std::cerr << "Error loading plugin: " << dlerror() << std::endl;
            return {};
        }
        lightHandle = handle; // ✅ FIX: sauvegarde du handle
    } else {
        handle = lightHandle;
    }

    typedef IPlugin *(*create_t)();
    dlerror(); // Clear previous error

    create_t createAmbientLight = (create_t)dlsym(handle, "createAmbientLight");
    const char *dlsym_error = dlerror();
    if (dlsym_error) {
        std::cerr << "Error loading symbol 'createAmbientLight': " << dlsym_error << std::endl;
        dlclose(handle);
        lightHandle = nullptr; // optionnel, évite réutilisation
        return {};
    }

    create_t createDirectionnalLight = (create_t)dlsym(handle, "createDirectionnalLights");
    dlsym_error = dlerror();
    if (dlsym_error) {
        std::cerr << "Error loading symbol 'createDirectionnalLights': " << dlsym_error << std::endl;
        dlclose(handle);
        lightHandle = nullptr;
        return {};
    }
    

    std::vector<IPlugin *> lights;
    IPlugin *ambientLight = createAmbientLight();
    IPlugin *directionalLight = createDirectionnalLight();

    if (ambientLight) lights.push_back(ambientLight);
    else std::cerr << "Error creating ambient light plugin." << std::endl;

    if (directionalLight) lights.push_back(directionalLight);
    else std::cerr << "Error creating directional light plugin." << std::endl;

    return lights;
}

IPlugin *PluginLoader::loadRenderPlugin(const std::string &path) {
    void *handle = dlopen(path.c_str(), RTLD_LAZY);
    
    if (!handle) {
        std::cerr << "Error loading plugin: " << dlerror() << std::endl;
        return nullptr;
    }
    dlerror();

    typedef IPlugin *(*create_t)();
    create_t createRenderPlugin = (create_t)dlsym(handle, "createRenderPlugin");
    const char *dlsym_error = dlerror();
    if (dlsym_error) {
        std::cerr << "Error loading symbol: " << dlsym_error << std::endl;
        dlclose(handle);
        return nullptr;
    }

    storeHandle(handle, path);

    IPlugin *plugin = createRenderPlugin();
    return plugin;
}

void *PluginLoader::loadSharedLibrary(const std::string &path, void *&handle) {
    if (!handle) {
        handle = dlopen(path.c_str(), RTLD_LAZY);
        if (!handle) {
            std::cerr << "Error loading plugin: " << dlerror() << std::endl;
            return nullptr;
        }
    }
    return handle;
}

void *PluginLoader::loadSymbol(void *handle, const std::string &symbolName) {
    dlerror(); // Clear previous error
    void *symbol = dlsym(handle, symbolName.c_str());
    const char *error = dlerror();
    if (error) {
        std::cerr << "Error loading symbol '" << symbolName << "': " << error << std::endl;
        return nullptr;
    }
    return symbol;
}

std::unordered_map<std::string, void **> PluginLoader::_factory = {
    {"./plugins/raytracer_plane.so", &PluginLoader::planeHandle},
    {"./plugins/raytracer_sphere.so", &PluginLoader::sphereHandle},
    {"./plugins/raytracer_cylinder.so", &PluginLoader::cylinderHandle},
    {"./plugins/raytracer_cone.so", &PluginLoader::coneHandle},
    {"./plugins/raytracer_cube.so", &PluginLoader::cubeHandle},
};

void *&PluginLoader::getInstance(const std::string &path) {
    auto it = _factory.find(path);

    if (it != _factory.end()) {
        return *(it->second);
    }

    throw RayTracerException("Error: Unknown plugin path " + path);
}

IPlugin *PluginLoader::createPrimitive(const std::string &libPath, const std::string &symbolName) {
    void *&handle = getInstance(libPath);
    void *lib = loadSharedLibrary(libPath, handle);
    if (!lib) return nullptr;

    using create_t = IPlugin *(*)();
    create_t createFunc = (create_t)loadSymbol(lib, symbolName);
    if (!createFunc) {
        dlclose(lib);
        handle = nullptr;
        return nullptr;
    }
    return createFunc();
}

void PluginLoader::unloadCamera(IPlugin *camera) {
    if (camera) {
        delete camera;
        this->camera = nullptr;
    }

    if (cameraHandle) {
        dlclose(cameraHandle);
        cameraHandle = nullptr;
    }
}

void PluginLoader::unloadRender(IPlugin *plugin) {
    if (plugin) {
        delete plugin;
        this->render = nullptr;
    }

    if (renderHandle) {
        dlclose(renderHandle);
        renderHandle = nullptr;
    }
}

void PluginLoader::unloadLights(std::vector<IPlugin *> lights) {
    for (auto &light : lights) {
        if (light) {
            delete light;
        }
    }
    this->lights.clear();

    if (lightHandle) {
        dlclose(lightHandle);
        lightHandle = nullptr;
    }
}

void PluginLoader::unloadPrimitives(std::vector<IPlugin *> primitive) {
    for (auto &prim : primitive) {
        if (prim) {
            delete prim;
        }
    }
    this->primitive.clear();

    if (planeHandle) {
        dlclose(planeHandle);
        planeHandle = nullptr;
    }
    if (sphereHandle) {
        dlclose(sphereHandle);
        sphereHandle = nullptr;
    }
    if (cylinderHandle) {
        dlclose(cylinderHandle);
        cylinderHandle = nullptr;
    }
    if (coneHandle) {
        dlclose(coneHandle);
        coneHandle = nullptr;
    }
    if (lightHandle) {
        dlclose(lightHandle);
        lightHandle = nullptr;
    }
    if (renderHandle) {
        dlclose(renderHandle);
        renderHandle = nullptr;
    }
    if (cubeHandle) {
        dlclose(cubeHandle);
        cubeHandle = nullptr;
    }
}
