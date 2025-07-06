/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Core
*/

#include "Core.hpp"

bool Core::isFileValid(const std::string &filename)
{
    // Check if the file exists and is valid
    std::ifstream file(filename);

    if (!file) {
        std::cerr << "Error: File not found or invalid format." << std::endl;
        return false;
    }
    // Check if the file is a .cfg file
    if (filename.substr(filename.find_last_of(".") + 1) != "cfg") {
        std::cerr << "Error: Invalid file format. Expected .cfg file." << std::endl;
        return false;
    }
    return true;
}

// Function to convert a vector of IPlugin pointers to a vector of ILight pointers
void Core::convertVectorPluginToVectorLight(std::vector<IPlugin*> &plugins, std::vector<ILight*> &lights)
{
    for (auto &plugin : plugins) {
        ILight *light = dynamic_cast<ILight*>(plugin);
        if (light) {
            lights.push_back(light);
        }
    }
}

// Function to convert a vector of IPlugin pointers to a vector of IPrimitive pointers
void Core::convertVectorPluginToVectorPrimitive(std::vector<IPlugin*> &plugins, std::vector<IPrimitive*> &primitives)
{
    for (auto &plugin : plugins) {
        IPrimitive *primitive = dynamic_cast<IPrimitive*>(plugin);
        if (primitive) {
            primitives.push_back(primitive);
        }
    }
}

void core_run(char *filename)
{
    Core core;
    
    if (!core.isFileValid(filename))
        throw ParsingException("Invalid file format");

    SceneLoader sceneLoader;
    if (!sceneLoader.checkCfgError(filename))
        throw ParsingException("Error loading scene configuration");

    // Load all plugins
    PluginLoader pluginLoader;

    // Camera plugin loading
    pluginLoader.camera = pluginLoader.loadCameraPlugin("./plugins/camera.so");
    if (!pluginLoader.camera) {
        throw ParsingException("Failed to load camera plugin."); // ToDo: Changer l'erreur
    }
    sceneLoader.loadCamera(pluginLoader.camera);

    pluginLoader.lights = pluginLoader.loadlightsPlugin("./plugins/lights.so");
    if (pluginLoader.lights.empty()) {
        throw ParsingException("Failed to load light plugin."); // ToDo: Changer l'erreur
    }
    sceneLoader.loadLights(pluginLoader.lights);

    pluginLoader.render = pluginLoader.loadRenderPlugin("./plugins/render.so");
    if (!pluginLoader.render) {
        throw ParsingException("Failed to load render plugin."); // ToDo: Changer l'erreur
    }
    sceneLoader.loadRender(pluginLoader.render);

    // Le parsing et loading se trouve dedans
    pluginLoader.primitive = sceneLoader.loadPrimitives(pluginLoader);
    if (pluginLoader.primitive.empty()) {
        throw ParsingException("Failed to load primitive plugin."); // ToDo: Changer l'erreur
    }

    // Display camera information
    RayTracer::ICamera *cam = sceneLoader.castPlugin<RayTracer::ICamera>(pluginLoader.camera);
    if (cam) {
        std::cout << "Camera Position: " << cam->getPosition().getX() << ", "
                  << cam->getPosition().getY() << ", " << cam->getPosition().getZ() << std::endl;
        std::cout << "Camera Resolution: " << cam->getWidth() << "x" << cam->getHeight() << std::endl;
        std::cout << "Camera Field of View: " << cam->getFieldOfView() << std::endl;
        std::cout << "Camera LookAt: " << cam->getLookAt().getX() << ", "
                  << cam->getLookAt().getY() << ", " << cam->getLookAt().getZ() << std::endl;
    } else {
        throw ParsingException("Failed to cast plugin to Camera. (In function core_run)");
    }
    // Display light information
    for (auto &light : pluginLoader.lights) {
        ILight *l = sceneLoader.castPlugin<ILight>(light);
        if (l) {
            std::cout << "Light Type: " << static_cast<int>(l->getLightType()) << std::endl;
            if (l->getLightType() == LightType::AMBIENT) {
                std::cout << "Ambient Light Intensity: " << l->getIntensity() << std::endl;

            } else if (l->getLightType() == LightType::DIRECTIONAL) {
                std::cout << "Directional Light Intensity: " << l->getIntensity() << std::endl;
                Math::Vector3D direction = l->getDirection();
                std::cout << "Directional Light Direction: " << direction.getX() << ", "
                          << direction.getY() << ", " << direction.getZ() << std::endl;
            }
        } else {
            throw ParsingException("Failed to cast plugin to Light. (In function core_run)");
        }
    }
    // Display render information
    IRenderer *renderer = sceneLoader.castPlugin<IRenderer>(pluginLoader.render);
    if (renderer) {
        std::cout << "Renderer Type: " << static_cast<int>(renderer->getRendererType()) << std::endl;
    } else {
        throw ParsingException("Failed to cast plugin to Renderer. (In function core_run)");
    }

    // Display primitive information
    for (auto &primitive : pluginLoader.primitive) {
        IPrimitive *p = sceneLoader.castPlugin<IPrimitive>(primitive);
        if (p) {
            std::cout << "Primitive Type: " << static_cast<int>(p->getPrimitiveType()) << std::endl;
            if (p->getPrimitiveType() == PrimitiveType::SPHERE) {
                std::cout << "\tPrimitive Position: " << p->getPosition().getX() << ", "
                      << p->getPosition().getY() << ", " << p->getPosition().getZ() << std::endl;
                std::cout << "\tPrimitive Radius: " << p->getRadius() << std::endl;
            } else if (p->getPrimitiveType() == PrimitiveType::PLANE) {
                std::cout << "\tPrimitive Position: " << p->getPosition().getX() << std::endl;
                Math::Vector3D normal = p->getNormal();
                std::cout << "\tPrimitive Normal: " << normal.getX() << ", "
                          << normal.getY() << ", " << normal.getZ() << std::endl;
            } else if (p->getPrimitiveType() == PrimitiveType::CYLINDER) {
                std::cout << "\tPrimitive Position: " << p->getPosition().getX() << ", "
                      << p->getPosition().getY() << ", " << p->getPosition().getZ() << std::endl;
                std::cout << "\tPrimitive Radius: " << p->getRadius() << std::endl;
                std::cout << "\tPrimitive Height: " << p->getHeight() << std::endl;
            } else if (p->getPrimitiveType() == PrimitiveType::CONE) {
                std::cout << "\tPrimitive Position: " << p->getPosition().getX() << ", "
                      << p->getPosition().getY() << ", " << p->getPosition().getZ() << std::endl;
                std::cout << "\tPrimitive Radius: " << p->getRadius() << std::endl;
                std::cout << "\tPrimitive Height: " << p->getHeight() << std::endl;
            } else if (p->getPrimitiveType() == PrimitiveType::CUBE) {
                std::cout << "\tPrimitive Position: " << p->getPosition().getX() << ", "
                      << p->getPosition().getY() << ", " << p->getPosition().getZ() << std::endl;
                std::cout << "\tPrimitive Height: " << p->getHeight() << std::endl;
                std::cout << "\tPrimitive Radius: " << p->getRadius() << std::endl;
            }
            Math::Color color = p->getColor();
            std::cout << "\tPrimitive Color: " << color.getR() << ", "
                      << color.getG() << ", " << color.getB() << std::endl;
        } else {
            throw ParsingException("Failed to cast plugin to Primitive. (In function core_run)");
        }
    }
    // Render the scene
    std::cout << "Rendering scene..." << std::endl;
    renderer->setCamera(cam);

    // Convert pluginLoader.lights (vector<IPlugin*>) to vector<ILight*>
    std::vector<ILight*> lightPtrs;
    core.convertVectorPluginToVectorLight(pluginLoader.lights, lightPtrs);
    renderer->setLights(lightPtrs);
    // Convert pluginLoader.primitives (vector<IPlugin*>) to vector<IPrimitive*>
    std::vector<IPrimitive*> primitivePtrs;
    core.convertVectorPluginToVectorPrimitive(pluginLoader.primitive, primitivePtrs);
    renderer->setPrimitives(primitivePtrs);

    // // Faire pivoter différentes primitives selon différents angles et axes
    // for (size_t i = 0; i < primitivePtrs.size(); i++) {
    //     if (primitivePtrs[i]->getPrimitiveType() == PrimitiveType::CYLINDER) {
    //         // Premier cylindre: rotation autour de l'axe X
    //         Math::Vector3D rotationAxisX(1, 0, 0);
    //         float angleX = 90.0f;
    //         primitivePtrs[i]->rotate(angleX, rotationAxisX);
    //         std::cout << "Rotated cylinder " << i << " by " << angleX << " degrees around X axis" << std::endl;
            
    //         // On continue la recherche pour trouver d'autres cylindres
    //     } else if (primitivePtrs[i]->getPrimitiveType() == PrimitiveType::CONE) {
    //         // Pour les cônes, rotation autour de l'axe Z
    //         Math::Vector3D rotationAxisZ(0, 0, 1);
    //         float angleZ = 60.0f;
    //         primitivePtrs[i]->rotate(angleZ, rotationAxisZ);
    //         std::cout << "Rotated cone " << i << " by " << angleZ << " degrees around Z axis" << std::endl;
    //     }
    // }

    //Translate all cones
    // for (size_t i = 0; i < primitivePtrs.size(); i++) {
    //     if (primitivePtrs[i]->getPrimitiveType() == PrimitiveType::CONE) {
    //         Math::Vector3D translation(100, 2, 300);
    //         primitivePtrs[i]->translate(translation);
    //         std::cout << "Translated cone " << i << " by (0, 0, 1)" << std::endl;
    //     }
    // }

    renderer->renderScene();
    /////////////////////////////////////:

    // --- Affichage SFML du .ppm ---
    int width = cam->getWidth();
    int height = cam->getHeight();
    std::ifstream ppm("output.ppm");
    std::string header;
    int maxVal;
    if (ppm) {
        ppm >> header >> width >> height >> maxVal;
        ppm.ignore(); // Skip single whitespace after header
        sf::Image image;
        image.create(width, height, sf::Color::Black);
        for (int y = 0; y < height; ++y) {
            for (int x = 0; x < width; ++x) {
                unsigned int r, g, b;
                ppm >> r >> g >> b;
                image.setPixel(x, y, sf::Color(r, g, b));
            }
        }
        sf::Texture texture;
        if (!texture.loadFromImage(image)) {
            std::cerr << "Erreur lors du chargement de l'image dans la texture." << std::endl;
            return;
        }
        sf::Sprite sprite(texture);
        sf::RenderWindow window(sf::VideoMode(width, height), "Raytracer Output", sf::Style::Titlebar | sf::Style::Close);
        while (window.isOpen()) {
            sf::Event event;
            while (window.pollEvent(event)) {
                if (event.type == sf::Event::Closed)
                    window.close();
            }
            window.clear(sf::Color::Black);
            window.draw(sprite);
            window.display();
        }
    } else {
        std::cerr << "Erreur lors de l'ouverture du fichier output.ppm" << std::endl;
    }

    pluginLoader.unloadCamera(pluginLoader.camera);
    pluginLoader.unloadRender(pluginLoader.render);
    pluginLoader.unloadLights(pluginLoader.lights);
    pluginLoader.unloadPrimitives(pluginLoader.primitive);
}
