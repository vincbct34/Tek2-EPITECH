/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** SceneLoader
*/

#include "SceneLoader.hpp"

bool SceneLoader::checkCfgError(const std::string &filename) {
    try {
        cfg.readFile(filename.c_str());
    } catch (const FileIOException &fioex) {
        std::cerr << "I/O error while reading file: " << filename << std::endl;
        return false;
    } catch (const ParseException &pex) {
        std::cerr << "Parse error at " << pex.getFile() << ":" << pex.getLine()
                  << " - " << pex.getError() << std::endl;
        return false;
    }
    return true;
}

void SceneLoader::loadCamera(IPlugin *plugin) {
    if (cfg.exists("camera")) {
        Setting &cameraSetting = cfg.lookup("camera");

        RayTracer::ICamera *camera = castPlugin<RayTracer::ICamera>(plugin);
        if (!camera) {
            throw ParsingException("Failed to cast plugin to Camera. (In function loadCamera)");
        }

        if (cameraSetting.exists("resolution")) {
            const Setting &resolution = cameraSetting.lookup("resolution");
        
            int width = resolution["width"];
            int height = resolution["height"];
        
            camera->setResolution(width, height);    
        }
        if (cameraSetting.exists("position")) {
            const Setting &position = cameraSetting.lookup("position");
        
            int x = position["x"];
            int y = position["y"];
            int z = position["z"];
        
            camera->setPosition(Math::Point3D(x, y, z));
        }
        if (cameraSetting.exists("fieldOfView")) {
            double fov = cameraSetting.lookup("fieldOfView");
            camera->setFieldOfView(fov);
        }
        if (cameraSetting.exists("lookAt")) {
            const Setting &lookAt = cameraSetting.lookup("lookAt");
        
            int x = lookAt["x"];
            int y = lookAt["y"];
            int z = lookAt["z"];
        
            camera->setLookAt(Math::Point3D(x, y, z));
        }
    } else {
        throw ParsingException("Camera configuration not found in the file.");
    }
}

void SceneLoader::loadLights(std::vector<IPlugin *> lights) {
    if (cfg.exists("lights")) {
        Setting &lightsSetting = cfg.lookup("lights");

        // Charger la lumière ambiante
        if (lightsSetting.exists("ambient")) {
            float ambientIntensity = lightsSetting["ambient"];
            ILight *ambientLight = castPlugin<ILight>(lights[0]);
            if (ambientLight) {
                ambientLight->setIntensity(ambientIntensity);
            } else {
                throw ParsingException("Failed to cast plugin to ILight.");
            }
        }

        // Charger les lumières directionnelles
        if (lightsSetting.exists("point")) {
            const Setting &pointLights = lightsSetting.lookup("point");
            for (int i = 0; i < pointLights.getLength(); ++i) {
                const Setting &light = pointLights[i];
                int x = light["x"];
                int y = light["y"];
                int z = light["z"];

                ILight *directionalLight = castPlugin<ILight>(lights[1]);
                (void)directionalLight;
                (void)x;
                (void)y;
                (void)z;
                if (directionalLight) {
                    directionalLight->setDirection(x, y, z);
                } else {
                    throw ParsingException("Failed to cast plugin to DirectionnalLights.");
                }
            }
        }
    } else {
        throw ParsingException("Lights configuration not found in the file.");
    }
}

void SceneLoader::loadRender(IPlugin *plugin) {
    if (cfg.exists("renderer")) {
        Setting &renderSetting = cfg.lookup("renderer");
        IRenderer *renderer = castPlugin<IRenderer>(plugin);
        if (!renderer) {
            throw ParsingException("Failed to cast plugin to Renderer. (In function loadRender)");
        }
        if (renderSetting.exists("type")) {
            std::string type = renderSetting["type"];
            if (type == "basic") {
                renderer->setRendererType(RendererType::BASIC);
            } else if (type == "file") {
                renderer->setRendererType(RendererType::FILE);
            } else if (type == "SFML") {
                renderer->setRendererType(RendererType::SFML);
            } else {
                throw ParsingException("Invalid renderer type.");
            }
        }
    }
}

void SceneLoader::fillSphere(IPlugin *spherePlugin, Setting &sphereSetting) {
    int x = sphereSetting["x"];
    int y = sphereSetting["y"];
    int z = sphereSetting["z"];
    int radius = sphereSetting["r"];
    const Setting &color = sphereSetting["color"];
    int red = color["r"];
    int green = color["g"];
    int blue = color["b"];
    

    IPrimitive *spherePrimitive = castPlugin<IPrimitive>(spherePlugin);
    if (spherePrimitive) {
        spherePrimitive->setPosition(Math::Point3D(x, y, z));
        spherePrimitive->setRadius(radius);
        Math::Color sphereColor = Math::Color(red, green, blue);
        spherePrimitive->setColor(sphereColor);
    } else {
        throw ParsingException("Failed to cast plugin to Sphere Primitive.");
    }
}

void SceneLoader::fillPlane(IPlugin *planePlugin, Setting &planeSetting) {
    std::string axis = planeSetting["axis"];
    int position = planeSetting["position"];
    const Setting &color = planeSetting["color"];
    int red = color["r"];
    int green = color["g"];
    int blue = color["b"];

    IPrimitive *planePrimitive = castPlugin<IPrimitive>(planePlugin);
    if (planePrimitive) {
        if (axis == "X") {
            planePrimitive->setNormal(Math::Vector3D(1, 0, 0));
        } else if (axis == "Y") {
            planePrimitive->setNormal(Math::Vector3D(0, 1, 0));
        } else if (axis == "Z") {
            planePrimitive->setNormal(Math::Vector3D(0, 0, 1));
        } else {
            throw ParsingException("Invalid axis value for plane.");
        }
        planePrimitive->setPosition(Math::Point3D(position, position, position));
        Math::Color planeColor = Math::Color(red, green, blue);
        planePrimitive->setColor(planeColor);
    } else {
        throw ParsingException("Failed to cast plugin to Plane Primitive.");
    }
}

void SceneLoader::fillCylinder(IPlugin *cylinderPlugin, Setting &cylinderSetting) {
    int x = cylinderSetting["x"];
    int y = cylinderSetting["y"];
    int z = cylinderSetting["z"];
    int radius = cylinderSetting["r"];
    int height = cylinderSetting["h"];
    const Setting &color = cylinderSetting["color"];
    int red = color["r"];
    int green = color["g"];
    int blue = color["b"];

    IPrimitive *cylinderPrimitive = castPlugin<IPrimitive>(cylinderPlugin);
    if (cylinderPrimitive) {
        cylinderPrimitive->setPosition(Math::Point3D(x, y, z));
        cylinderPrimitive->setRadius(radius);
        cylinderPrimitive->setHeight(height);
        Math::Color cylinderColor = Math::Color(red, green, blue);
        cylinderPrimitive->setColor(cylinderColor);
    } else {
        throw ParsingException("Failed to cast plugin to Cylinder Primitive.");
    }
}

void SceneLoader::fillCone(IPlugin *conePlugin, Setting &coneSetting) {
    int x = coneSetting["x"];
    int y = coneSetting["y"];
    int z = coneSetting["z"];
    int radius = coneSetting["r"];
    int height = coneSetting["h"];
    const Setting &color = coneSetting["color"];
    int red = color["r"];
    int green = color["g"];
    int blue = color["b"];

    IPrimitive *conePrimitive = castPlugin<IPrimitive>(conePlugin);
    if (conePrimitive) {
        conePrimitive->setPosition(Math::Point3D(x, y, z));
        conePrimitive->setRadius(radius);
        conePrimitive->setHeight(height);
        Math::Color coneColor = Math::Color(red, green, blue);
        conePrimitive->setColor(coneColor);
    } else {
        throw ParsingException("Failed to cast plugin to Cone Primitive.");
    }
}

void SceneLoader::fillCube(IPlugin *cubePlugin, Setting &cubeSetting) {
    int x = cubeSetting["x"];
    int y = cubeSetting["y"];
    int z = cubeSetting["z"];
    int h = cubeSetting["h"];
    int r = cubeSetting["r"];
    const Setting &color = cubeSetting["color"];
    int red = color["r"];
    int green = color["g"];
    int blue = color["b"];

    IPrimitive *cubePrimitive = castPlugin<IPrimitive>(cubePlugin);
    if (cubePrimitive) {
        cubePrimitive->setPosition(Math::Point3D(x, y, z));
        cubePrimitive->setHeight(h);
        cubePrimitive->setRadius(r);
        Math::Color cubeColor = Math::Color(red, green, blue);
        cubePrimitive->setColor(cubeColor);
    } else {
        throw ParsingException("Failed to cast plugin to Cube Primitive.");
    }
}

std::vector<IPlugin *> SceneLoader::loadPrimitives(PluginLoader &pluginLoader) {
    std::vector<IPlugin *> primitives;

    if (cfg.exists("primitives")) {
        Setting &primitivesSetting = cfg.lookup("primitives");

        if (primitivesSetting.exists("spheres")) {
            const Setting &spheres = primitivesSetting.lookup("spheres");
            for (int i = 0; i < spheres.getLength(); ++i) {                
                IPlugin *sphere = pluginLoader.createPrimitive("./plugins/raytracer_sphere.so", "createSpherePrimitive");
                if (!sphere)
                    throw ParsingException("Failed to load sphere plugin.");
                fillSphere(sphere, spheres[i]);
                primitives.push_back(sphere);
            }
        }
        if (primitivesSetting.exists("planes")) {
            const Setting &planes = primitivesSetting.lookup("planes");
            for (int i = 0; i < planes.getLength(); ++i) {
                IPlugin *plane = pluginLoader.createPrimitive("./plugins/raytracer_plane.so", "createPlanePrimitive");
                if (!plane)
                    throw ParsingException("Failed to load plane plugin.");
                fillPlane(plane, planes[i]);
                primitives.push_back(plane);
            }
        }
        if (primitivesSetting.exists("cylinders")) {
            const Setting &cylinders = primitivesSetting.lookup("cylinders");
            for (int i = 0; i < cylinders.getLength(); ++i) {
                IPlugin *cylinder = pluginLoader.createPrimitive("./plugins/raytracer_cylinder.so", "createCylinderPrimitive");
                if (!cylinder)
                    throw ParsingException("Failed to load cylinder plugin.");
                fillCylinder(cylinder, cylinders[i]);
                primitives.push_back(cylinder);
            }
        }
        if (primitivesSetting.exists("cones")) {
            const Setting &cones = primitivesSetting.lookup("cones");
            for (int i = 0; i < cones.getLength(); ++i) {
                IPlugin *cone = pluginLoader.createPrimitive("./plugins/raytracer_cone.so", "createConePrimitive");
                if (!cone)
                    throw ParsingException("Failed to load cone plugin.");
                fillCone(cone, cones[i]);
                primitives.push_back(cone);
            }
        }
        if (primitivesSetting.exists("cubes")) {
            const Setting &cubes = primitivesSetting.lookup("cubes");
            for (int i = 0; i < cubes.getLength(); ++i) {
                IPlugin *cube = pluginLoader.createPrimitive("./plugins/raytracer_cube.so", "createCubePrimitive");
                if (!cube)
                    throw ParsingException("Failed to load cube plugin.");
                fillCube(cube, cubes[i]);
                primitives.push_back(cube);
            }
        }
    } else {
        throw ParsingException("Primitives configuration not found in the file.");
    }
    return primitives;
}