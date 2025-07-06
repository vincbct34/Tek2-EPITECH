/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Cube
*/

#include "Cube.hpp"

RayTracer::Cube::Cube() 
    : center(Math::Point3D(0, 0, 0)), 
      size(2.0f), 
      color(Math::Color(255, 255, 255)),
      xAxis(Math::Vector3D(1, 0, 0)),
      yAxis(Math::Vector3D(0, 1, 0)),
      zAxis(Math::Vector3D(0, 0, 1))
{
}

bool RayTracer::Cube::hits(const Ray &ray) const
{
    // Direction du rayon
    Math::Vector3D dir = ray.getDirection();
    
    // Origine du rayon relative au centre du cube
    Math::Vector3D ori(
        ray.getOrigin().getX() - center.getX(),
        ray.getOrigin().getY() - center.getY(),
        ray.getOrigin().getZ() - center.getZ()
    );
    
    // Demi-taille du cube
    float halfSize = size / 2.0f;
    
    // Calcul des intersections avec les plans des faces
    float tmin = -INFINITY;
    float tmax = INFINITY;
    
    // Axe X
    if (std::abs(dir.getX()) < 1e-8) {
        // Rayon parallèle à la face
        if (ori.getX() > halfSize || ori.getX() < -halfSize)
            return false;
    } else {
        float invD = 1.0f / dir.getX();
        float t1 = (-halfSize - ori.getX()) * invD;
        float t2 = (halfSize - ori.getX()) * invD;
        
        if (t1 > t2) std::swap(t1, t2);
        tmin = std::max(tmin, t1);
        tmax = std::min(tmax, t2);
        
        if (tmin > tmax) return false;
    }
    
    // Axe Y
    if (std::abs(dir.getY()) < 1e-8) {
        // Rayon parallèle à la face
        if (ori.getY() > halfSize || ori.getY() < -halfSize)
            return false;
    } else {
        float invD = 1.0f / dir.getY();
        float t1 = (-halfSize - ori.getY()) * invD;
        float t2 = (halfSize - ori.getY()) * invD;
        
        if (t1 > t2) std::swap(t1, t2);
        tmin = std::max(tmin, t1);
        tmax = std::min(tmax, t2);
        
        if (tmin > tmax) return false;
    }
    
    // Axe Z
    if (std::abs(dir.getZ()) < 1e-8) {
        // Rayon parallèle à la face
        if (ori.getZ() > halfSize || ori.getZ() < -halfSize)
            return false;
    } else {
        float invD = 1.0f / dir.getZ();
        float t1 = (-halfSize - ori.getZ()) * invD;
        float t2 = (halfSize - ori.getZ()) * invD;
        
        if (t1 > t2) std::swap(t1, t2);
        tmin = std::max(tmin, t1);
        tmax = std::min(tmax, t2);
        
        if (tmin > tmax) return false;
    }
    
    return tmax > 0; // Au moins une intersection est devant l'origine du rayon
}

bool RayTracer::Cube::intersect(const Ray &ray, RayTracer::HitRecord &rec) const
{
    // Direction du rayon
    Math::Vector3D dir = ray.getDirection();
    
    // Origine du rayon relative au centre du cube
    Math::Vector3D ori(
        ray.getOrigin().getX() - center.getX(),
        ray.getOrigin().getY() - center.getY(),
        ray.getOrigin().getZ() - center.getZ()
    );
    
    // Demi-taille du cube
    float halfSize = size / 2.0f;
    
    // Calcul des intersections avec les plans des faces
    float tmin = -INFINITY;
    float tmax = INFINITY;
    
    int hitAxis = -1;
    float hitSign = 0.0f;
    
    // Axe X
    if (std::abs(dir.getX()) < 1e-8) {
        // Rayon parallèle à la face
        if (ori.getX() > halfSize || ori.getX() < -halfSize)
            return false;
    } else {
        float invD = 1.0f / dir.getX();
        float t1 = (-halfSize - ori.getX()) * invD;
        float t2 = (halfSize - ori.getX()) * invD;
        
        if (t1 > t2) std::swap(t1, t2);
        
        if (t1 > tmin) {
            tmin = t1;
            hitAxis = 0;
            hitSign = (invD < 0) ? 1.0f : -1.0f;
        }
        
        tmax = std::min(tmax, t2);
        if (tmin > tmax) return false;
    }
    
    // Axe Y
    if (std::abs(dir.getY()) < 1e-8) {
        // Rayon parallèle à la face
        if (ori.getY() > halfSize || ori.getY() < -halfSize)
            return false;
    } else {
        float invD = 1.0f / dir.getY();
        float t1 = (-halfSize - ori.getY()) * invD;
        float t2 = (halfSize - ori.getY()) * invD;
        
        if (t1 > t2) std::swap(t1, t2);
        
        if (t1 > tmin) {
            tmin = t1;
            hitAxis = 1;
            hitSign = (invD < 0) ? 1.0f : -1.0f;
        }
        
        tmax = std::min(tmax, t2);
        if (tmin > tmax) return false;
    }
    
    // Axe Z
    if (std::abs(dir.getZ()) < 1e-8) {
        // Rayon parallèle à la face
        if (ori.getZ() > halfSize || ori.getZ() < -halfSize)
            return false;
    } else {
        float invD = 1.0f / dir.getZ();
        float t1 = (-halfSize - ori.getZ()) * invD;
        float t2 = (halfSize - ori.getZ()) * invD;
        
        if (t1 > t2) std::swap(t1, t2);
        
        if (t1 > tmin) {
            tmin = t1;
            hitAxis = 2;
            hitSign = (invD < 0) ? 1.0f : -1.0f;
        }
        
        tmax = std::min(tmax, t2);
        if (tmin > tmax) return false;
    }
    
    // Si l'intersection la plus proche est derrière l'origine du rayon
    if (tmin < 0) {
        if (tmax < 0) return false;
        tmin = tmax;
        
        // Recalculer l'axe et le signe pour la seconde intersection
        float t;
        
        // Axe X
        if (dir.getX() != 0) {
            float invD = 1.0f / dir.getX();
            t = (halfSize * ((invD < 0) ? -1.0f : 1.0f) - ori.getX()) * invD;
            if (t >= 0 && t <= tmin) {
                tmin = t;
                hitAxis = 0;
                hitSign = (invD < 0) ? -1.0f : 1.0f;
            }
        }
        
        // Axe Y
        if (dir.getY() != 0) {
            float invD = 1.0f / dir.getY();
            t = (halfSize * ((invD < 0) ? -1.0f : 1.0f) - ori.getY()) * invD;
            if (t >= 0 && t <= tmin) {
                tmin = t;
                hitAxis = 1;
                hitSign = (invD < 0) ? -1.0f : 1.0f;
            }
        }
        
        // Axe Z
        if (dir.getZ() != 0) {
            float invD = 1.0f / dir.getZ();
            t = (halfSize * ((invD < 0) ? -1.0f : 1.0f) - ori.getZ()) * invD;
            if (t >= 0 && t <= tmin) {
                tmin = t;
                hitAxis = 2;
                hitSign = (invD < 0) ? -1.0f : 1.0f;
            }
        }
    }
    
    // Si aucune intersection valide n'a été trouvée
    if (tmin < 0 || hitAxis == -1) return false;
    
    // Calculer le point d'intersection
    rec.t = tmin;
    rec.point = Math::Point3D(
        ray.getOrigin().getX() + tmin * ray.getDirection().getX(),
        ray.getOrigin().getY() + tmin * ray.getDirection().getY(),
        ray.getOrigin().getZ() + tmin * ray.getDirection().getZ()
    );
    
    // Calculer la normale au point d'intersection
    switch (hitAxis) {
        case 0: // Axe X
            rec.normal = Math::Vector3D(hitSign * xAxis.getX(), hitSign * xAxis.getY(), hitSign * xAxis.getZ());
            break;
        case 1: // Axe Y
            rec.normal = Math::Vector3D(hitSign * yAxis.getX(), hitSign * yAxis.getY(), hitSign * yAxis.getZ());
            break;
        case 2: // Axe Z
            rec.normal = Math::Vector3D(hitSign * zAxis.getX(), hitSign * zAxis.getY(), hitSign * zAxis.getZ());
            break;
    }
    
    rec.color = color;
    return true;
}

void RayTracer::Cube::translate(const Math::Vector3D &translation)
{
    center = Math::Point3D(
        center.getX() + translation.getX(),
        center.getY() + translation.getY(),
        center.getZ() + translation.getZ()
    );
}

void RayTracer::Cube::rotate(float angle, const Math::Vector3D &axis)
{
    // Mettre en place la rotation une fois que vous aurez implémenté la classe Matrix
    // ou utilisez les fonctions de rotation directement ici
    
    // Pour l'instant, cette fonction est vide
    (void)angle;
    (void)axis;
}

extern "C" IPlugin *createCubePrimitive() {
    return new RayTracer::Cube();
} 