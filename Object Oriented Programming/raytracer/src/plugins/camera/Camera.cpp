/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Camera
*/

#include "Camera.hpp"

// Constructor
namespace RayTracer {
    Camera::Camera() : origin(Math::Point3D(0, 0, 0)), 
                       lookAt_(Math::Point3D(0, 0, 1)),
                       screen(Math::Rectangle3D(
        Math::Point3D(-1, -1, 1),
        Math::Vector3D(2, 0, 0),
        Math::Vector3D(0, 2, 0)
    )), fieldOfView(90) {
        // Calcul dynamique du plan écran pour la perspective
        float d = 1.0f;
        float aspect = 16.0f / 9.0f; // À remplacer dynamiquement si possible
        float fovRad = fieldOfView * M_PI / 180.0f;
        float screenHeight = 2.0f * d * tan(fovRad / 2.0f);
        float screenWidth = screenHeight * aspect;
        screen = Math::Rectangle3D(
            Math::Point3D(-screenWidth/2, -screenHeight/2, d),
            Math::Vector3D(screenWidth, 0, 0),
            Math::Vector3D(0, screenHeight, 0)
        );
    }

    Camera::Camera(const Math::Point3D& origin, const Math::Rectangle3D& screen, float fov)
        : origin(origin),
          lookAt_(Math::Point3D(0, 0, 1)),
          screen(screen),
          fieldOfView(fov) {
    }

    // Rotate the camera around the axis
    void Camera::rotate(float angle, const Math::Vector3D &axis) {
        // Convertir l'angle en radians
        float radians = angle * M_PI / 180.0f;
        float cosA = cos(radians);
        float sinA = sin(radians);
        
        // Normaliser l'axe de rotation
        float axisLength = sqrt(axis.getX() * axis.getX() + 
                                axis.getY() * axis.getY() + 
                                axis.getZ() * axis.getZ());
        
        float nx = axis.getX() / axisLength;
        float ny = axis.getY() / axisLength;
        float nz = axis.getZ() / axisLength;
        
        // Formule de rotation de Rodrigues pour la caméra
        // v_rot = v * cos(angle) + (axis × v) * sin(angle) + axis * (axis · v) * (1 - cos(angle))
        
        // Calculer le produit scalaire
        float dotProduct = nx * lookAt_.getX() + ny * lookAt_.getY() + nz * lookAt_.getZ();
        
        // Calculer le produit vectoriel
        float crossX = ny * lookAt_.getZ() - nz * lookAt_.getY();
        float crossY = nz * lookAt_.getX() - nx * lookAt_.getZ();
        float crossZ = nx * lookAt_.getY() - ny * lookAt_.getX();
        
        // Calculer le nouvel axe
        lookAt_ = Math::Point3D(
            lookAt_.getX() * cosA + crossX * sinA + nx * dotProduct * (1 - cosA),
            lookAt_.getY() * cosA + crossY * sinA + ny * dotProduct * (1 - cosA),
            lookAt_.getZ() * cosA + crossZ * sinA + nz * dotProduct * (1 - cosA)
        );
    }
    // Translate the camera
    void Camera::translate(const Math::Vector3D &translation) {
        origin = Math::Point3D(
            origin.getX() + translation.getX(),
            origin.getY() + translation.getY(),
            origin.getZ() + translation.getZ()
        );
    }

    void Camera::updateScreen() {
        // Calculer la direction de la caméra (du point de vue vers le lookAt)
        Math::Vector3D forward = Math::Vector3D(
            lookAt_.getX() - origin.getX(),
            lookAt_.getY() - origin.getY(),
            lookAt_.getZ() - origin.getZ()
        );
        
        float forwardLength = std::sqrt(forward.getX()*forward.getX() + 
                                       forward.getY()*forward.getY() + 
                                       forward.getZ()*forward.getZ());
        
        // Éviter la division par zéro
        if (forwardLength > 0.0001f) {
            forward = Math::Vector3D(
                forward.getX() / forwardLength,
                forward.getY() / forwardLength,
                forward.getZ() / forwardLength
            );
        } else {
            // Si lookAt est trop proche de la caméra, utiliser une direction par défaut (vers l'avant)
            forward = Math::Vector3D(0, 0, 1);
        }

        // Calculer les vecteurs right et up
        Math::Vector3D up(0, 1, 0); // Vecteur up par défaut
        
        // Si forward est presque parallèle à up, choisir un up différent
        float dotProductWithUp = forward.getX() * up.getX() + forward.getY() * up.getY() + forward.getZ() * up.getZ();
        if (std::abs(dotProductWithUp) > 0.999f) {
            up = Math::Vector3D(1, 0, 0);
        }
        
        // Calculer le vecteur right perpendiculaire à forward et up (produit vectoriel)
        Math::Vector3D right = Math::Vector3D(
            forward.getY() * up.getZ() - forward.getZ() * up.getY(),
            forward.getZ() * up.getX() - forward.getX() * up.getZ(),
            forward.getX() * up.getY() - forward.getY() * up.getX()
        );
        
        // Normaliser right
        float rightLength = std::sqrt(right.getX()*right.getX() + 
                                     right.getY()*right.getY() + 
                                     right.getZ()*right.getZ());
        
        if (rightLength > 0.0001f) {
            right = Math::Vector3D(
                right.getX() / rightLength,
                right.getY() / rightLength,
                right.getZ() / rightLength
            );
        }
        
        // Recalculer up pour être perpendiculaire à forward et right (produit vectoriel)
        up = Math::Vector3D(
            right.getY() * forward.getZ() - right.getZ() * forward.getY(),
            right.getZ() * forward.getX() - right.getX() * forward.getZ(),
            right.getX() * forward.getY() - right.getY() * forward.getX()
        );
        
        // Normaliser up (en théorie il est déjà normalisé mais par précaution)
        float upLength = std::sqrt(up.getX()*up.getX() + up.getY()*up.getY() + up.getZ()*up.getZ());
        if (upLength > 0.0001f) {
            up = Math::Vector3D(
                up.getX() / upLength,
                up.getY() / upLength,
                up.getZ() / upLength
            );
        }

        // Calculer les dimensions du plan de l'écran en fonction du FOV
        float d = 1.0f; // Distance du plan de l'écran
        float aspect = static_cast<float>(width) / static_cast<float>(height);
        float fovRad = fieldOfView * M_PI / 180.0f;
        float screenHeight = 2.0f * d * tan(fovRad / 2.0f);
        float screenWidth = screenHeight * aspect;
        
        // Point central du plan de l'écran (à distance d de la caméra dans la direction forward)
        Math::Point3D screenCenter = Math::Point3D(
            origin.getX() + d * forward.getX(),
            origin.getY() + d * forward.getY(),
            origin.getZ() + d * forward.getZ()
        );
        
        // Point en bas à gauche du plan de l'écran
        Math::Point3D bottomLeft = Math::Point3D(
            screenCenter.getX() - (screenWidth/2) * right.getX() - (screenHeight/2) * up.getX(),
            screenCenter.getY() - (screenWidth/2) * right.getY() - (screenHeight/2) * up.getY(),
            screenCenter.getZ() - (screenWidth/2) * right.getZ() - (screenHeight/2) * up.getZ()
        );
        
        // Mise à jour du plan de l'écran avec les nouveaux vecteurs
        screen = Math::Rectangle3D(
            bottomLeft,
            Math::Vector3D(screenWidth * right.getX(), screenWidth * right.getY(), screenWidth * right.getZ()),
            Math::Vector3D(screenHeight * up.getX(), screenHeight * up.getY(), screenHeight * up.getZ())
        );
    }

    // Setters and Getters for camera resolution
    void Camera::setResolution(int width, int height) {
        this->width = width;
        this->height = height;
        updateScreen();
    }

    void Camera::getResolution(int &width, int &height) const {
        width = this->width;
        height = this->height;
    }

    Math::Point3D Camera::getPosition() const {
        return origin;
    }

    void Camera::setFieldOfView(float fov) {
        fieldOfView = fov;
        updateScreen();
    }

    float Camera::getFieldOfView() const {
        return fieldOfView;
    }

    void Camera::setLookAt(const Math::Point3D &lookAt) {
        lookAt_ = lookAt;
        
        // Calculer le vecteur de direction (forward) entre la caméra et le point lookAt
        Math::Vector3D forward = Math::Vector3D(
            lookAt_.getX() - origin.getX(),
            lookAt_.getY() - origin.getY(),
            lookAt_.getZ() - origin.getZ()
        );
        
        // Normaliser le vecteur forward
        float forwardLength = std::sqrt(forward.getX()*forward.getX() + 
                                       forward.getY()*forward.getY() + 
                                       forward.getZ()*forward.getZ());
        
        // Éviter la division par zéro
        if (forwardLength < 0.0001f) {
            return; // Si le lookAt est trop proche de la caméra, ne pas changer l'orientation
        }
        
        forward = Math::Vector3D(
            forward.getX() / forwardLength,
            forward.getY() / forwardLength,
            forward.getZ() / forwardLength
        );
        
        // Mettre à jour le plan de l'écran en fonction de la nouvelle orientation
        updateScreen();
    }

    Ray Camera::rayAt(double u, double v) const {
        // Le plan d'écran est déjà orienté correctement, on peut donc simplement
        // calculer le point sur le plan d'écran en fonction des coordonnées u,v
        
        // Calculer le point sur le plan d'écran
        Math::Point3D screenPoint = Math::Point3D(
            screen.origin.getX() + u * screen.bottom_side.getX() + v * screen.left_side.getX(),
            screen.origin.getY() + u * screen.bottom_side.getY() + v * screen.left_side.getY(),
            screen.origin.getZ() + u * screen.bottom_side.getZ() + v * screen.left_side.getZ()
        );
        
        // Calculer la direction du rayon (de l'origine de la caméra vers le point sur l'écran)
        Math::Vector3D direction = Math::Vector3D(
            screenPoint.getX() - origin.getX(),
            screenPoint.getY() - origin.getY(),
            screenPoint.getZ() - origin.getZ()
        );
        
        // Normaliser la direction
        float dirLength = std::sqrt(direction.getX()*direction.getX() + 
                                   direction.getY()*direction.getY() + 
                                   direction.getZ()*direction.getZ());
        
        if (dirLength > 0.0001f) {
            direction = Math::Vector3D(
                direction.getX() / dirLength,
                direction.getY() / dirLength,
                direction.getZ() / dirLength
            );
        }
        
        // Créer et retourner le rayon
        return Ray(origin, direction);
    }

    extern "C" IPlugin *createCameraPlugin() {
        return new Camera();
    }
}