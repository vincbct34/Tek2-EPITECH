/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Sphere
*/

#include "Sphere.hpp"

RayTracer::Sphere::Sphere() : center(Math::Point3D(0, 0, 0)), radius(1.0f), color(Math::Color(255, 255, 255)) {
}

bool RayTracer::Sphere::hits(const Ray &ray) const {
    Math::Vector3D oc = Math::Vector3D(ray.getOrigin().getX() - center.getX(),
                                       ray.getOrigin().getY() - center.getY(),
                                       ray.getOrigin().getZ() - center.getZ());

    double a = ray.getDirection().dot(ray.getDirection());
    double b = 2.0 * oc.dot(ray.getDirection());
    double c = oc.dot(oc) - radius * radius;
    double discriminant = b * b - 4 * a * c;

    return discriminant >= 0;
}

bool RayTracer::Sphere::intersect(const Ray &ray, RayTracer::HitRecord &rec) const {
    Math::Vector3D oc = Math::Vector3D(ray.getOrigin().getX() - center.getX(),
                                       ray.getOrigin().getY() - center.getY(),
                                       ray.getOrigin().getZ() - center.getZ());
    double a = ray.getDirection().dot(ray.getDirection());
    double b = 2.0 * oc.dot(ray.getDirection());
    double c = oc.dot(oc) - radius * radius;
    double discriminant = b * b - 4 * a * c;
    if (discriminant < 0)
        return false;
    double sqrtDisc = sqrt(discriminant);
    double t1 = (-b - sqrtDisc) / (2.0 * a);
    double t2 = (-b + sqrtDisc) / (2.0 * a);
    double t = (t1 >= 0) ? t1 : t2;
    if (t < 0)
        return false;
    rec.t = t;
    rec.point = Math::Point3D(
        ray.getOrigin().getX() + t * ray.getDirection().getX(),
        ray.getOrigin().getY() + t * ray.getDirection().getY(),
        ray.getOrigin().getZ() + t * ray.getDirection().getZ()
    );
    rec.normal = Math::Vector3D(
        (rec.point.getX() - center.getX()) / radius,
        (rec.point.getY() - center.getY()) / radius,
        (rec.point.getZ() - center.getZ()) / radius
    );
    rec.color = color;
    return true;
}

void RayTracer::Sphere::translate(const Math::Vector3D &translation) {
    center = Math::Point3D(center.getX() + translation.getX(),
                           center.getY() + translation.getY(),
                           center.getZ() + translation.getZ());
}

void RayTracer::Sphere::rotate(float angle, const Math::Vector3D &axis) {
    // Create a rotation matrix
    Math::Matrix4x4 rotMatrix;
    rotMatrix.rotationMatrix(angle, axis);
    
    // For a sphere, rotation around its center doesn't change its appearance
    // But to support rotating around a different axis, we need to:
    
    // 1. Calculate center position relative to the rotation axis origin
    Math::Point3D origin(0, 0, 0); // Assuming rotation around the world origin
    
    // 2. Apply rotation to the center
    center = rotMatrix.applyToPoint(center);
    
    // Note: For a sphere, we don't need to rotate any direction vectors
    // as it looks the same from all angles
}


extern "C" IPlugin *createSpherePrimitive() {
    return new RayTracer::Sphere();
}
