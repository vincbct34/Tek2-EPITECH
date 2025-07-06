/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** BasicRenderer
*/

#include "BasicRenderer.hpp"

BasicRenderer::BasicRenderer() :
    camera(),
    lights(),
    primitives() {
}

void BasicRenderer::setCamera(RayTracer::ICamera *camera) {
    this->camera = camera;
}

void BasicRenderer::setLights(std::vector<ILight *> &lights) {
    this->lights = lights;
}

void BasicRenderer::setPrimitives(std::vector<IPrimitive *> &primitives) {
    this->primitives = primitives;
}

void BasicRenderer::renderScene() {
    if (!camera) {
        std::cerr << "No camera set for rendering." << std::endl;
        return;
    }

    int width = camera->getWidth();
    int height = camera->getHeight();
    std::ofstream ppm("output.ppm");
    ppm << "P3\n" << width << " " << height << "\n255\n";

    int hitCount = 0;
    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            double u = static_cast<double>(x) / width;
            double v = static_cast<double>(y) / height;
            RayTracer::Ray ray = camera->rayAt(u, v);

            float r = 0, g = 0, b = 0;
            double closest_t = 1e30;
            RayTracer::HitRecord rec;
            bool found = false;
            for (auto *primitive : primitives) {
                RayTracer::HitRecord temp_rec;
                if (primitive && primitive->intersect(ray, temp_rec)) {
                    if (temp_rec.t < closest_t && temp_rec.t > 0) {
                        closest_t = temp_rec.t;
                        rec = temp_rec;
                        found = true;
                    }
                }
            }
            if (found) {
                hitCount++;
                r = rec.color.getR();
                g = rec.color.getG();
                b = rec.color.getB();

                // Coefficients de matériau
                const float k_ambient = 0.2f;   // Éclairage ambiant de base
                const float k_diffuse = 0.8f;   // Éclairage diffus principal

                float ambient = 0.0f;
                float diffuse = 0.0f;

                for (auto *light : lights) {
                    if (light->getLightType() == LightType::AMBIENT) {
                        ambient += light->getIntensity() * k_ambient;
                    } else if (light->getLightType() == LightType::DIRECTIONAL) {
                        Math::Vector3D lightDir = light->getDirection();
                        float len = std::sqrt(lightDir.getX()*lightDir.getX() + lightDir.getY()*lightDir.getY() + lightDir.getZ()*lightDir.getZ());
                        if (len > 0.0001f) {
                            lightDir = Math::Vector3D(lightDir.getX()/len, lightDir.getY()/len, lightDir.getZ()/len);
                        }

                        // Rayon d'ombre
                        Math::Vector3D shadowDir(-lightDir.getX(), -lightDir.getY(), -lightDir.getZ());
                        Math::Point3D shadowOrigin(
                            rec.point.getX() + 0.001f * shadowDir.getX(),
                            rec.point.getY() + 0.001f * shadowDir.getY(),
                            rec.point.getZ() + 0.001f * shadowDir.getZ()
                        );
                        RayTracer::Ray shadowRay(shadowOrigin, shadowDir);

                        // Vérification des ombres
                        bool inShadow = false;
                        for (auto *prim : primitives) {
                            RayTracer::HitRecord shadowRec;
                            if (prim != nullptr && prim->intersect(shadowRay, shadowRec)) {
                                if (shadowRec.t > 0.001f) {
                                    inShadow = true;
                                    break;
                                }
                            }
                        }

                        if (!inShadow) {
                            // Calcul de la réflexion diffuse (Lambert)
                            float lambert = rec.normal.dot(shadowDir);
                            lambert = std::max(0.0f, lambert);
                            diffuse += lambert * light->getIntensity() * k_diffuse;
                        }
                    }
                }

                // Combinaison des composantes d'éclairage
                float shade = std::max(0.0f, ambient + diffuse);
                shade = std::min(1.0f, shade);

                // Application de l'ombrage
                r *= shade;
                g *= shade;
                b *= shade;
            }
            ppm << static_cast<int>(r) << " " << static_cast<int>(g) << " " << static_cast<int>(b) << " ";
        }
        ppm << "\n";
    }
    ppm.close();
    std::cout << "Image written to output.ppm" << std::endl;
    std::cout << "Nombre de pixels touchant une primitive : " << hitCount << std::endl;
}

extern "C" IPlugin *createRenderPlugin() {
    return new BasicRenderer();
}
