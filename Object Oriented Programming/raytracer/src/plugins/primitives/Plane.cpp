/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Plane
*/

#include "Plane.hpp"

RayTracer::Plane::Plane() : point(Math::Point3D(0, 0, 0)), normal(Math::Vector3D(0, 1, 0)), color(Math::Color(255, 255, 255)) {
    // Constructor implementation
}

// ...existing code...
bool RayTracer::Plane::hits(const RayTracer::Ray &ray) const {
    float denom = normal.dot(ray.getDirection());
    if (fabs(denom) > 1e-6) { // Le rayon n'est pas parallèle au plan
        Math::Vector3D p0l0(
            point.getX() - ray.getOrigin().getX(),
            point.getY() - ray.getOrigin().getY(),
            point.getZ() - ray.getOrigin().getZ()
        );
        float t = p0l0.dot(normal) / denom;
        return (t >= 0); // Intersection devant l'origine du rayon
    }
    return false;
}

void RayTracer::Plane::translate(const Math::Vector3D &translation) {
    point = Math::Point3D(point.getX() + translation.getX(),
                          point.getY() + translation.getY(),
                          point.getZ() + translation.getZ());
}

void RayTracer::Plane::rotate(float angle, const Math::Vector3D &axis) {
    // To implement: rotation logic
    (void)angle;
    (void)axis;
}

bool RayTracer::Plane::intersect(const RayTracer::Ray &ray, RayTracer::HitRecord &rec) const {
    float denom = normal.dot(ray.getDirection());
    if (fabs(denom) > 1e-6) { // Le rayon n'est pas parallèle au plan
        Math::Vector3D p0l0(
            point.getX() - ray.getOrigin().getX(),
            point.getY() - ray.getOrigin().getY(),
            point.getZ() - ray.getOrigin().getZ()
        );
        float t = p0l0.dot(normal) / denom;
        if (t >= 0) { // Intersection devant l'origine du rayon
            rec.t = t;
            rec.point = Math::Point3D(
                ray.getOrigin().getX() + t * ray.getDirection().getX(),
                ray.getOrigin().getY() + t * ray.getDirection().getY(),
                ray.getOrigin().getZ() + t * ray.getDirection().getZ()
            );
            rec.normal = normal;
            rec.color = color;
            return true;
        }
    }
    return false;
}

extern "C" IPlugin *createPlanePrimitive() {
    return new RayTracer::Plane();
}
