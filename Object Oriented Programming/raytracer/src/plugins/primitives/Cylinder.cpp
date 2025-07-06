/*
** EPITECH PROJECT, 2025
** RayTracer [WSL: Ubuntu-24.04]
** File description:
** Cylinder
*/

#include "Cylinder.hpp"

namespace RayTracer {
    Cylinder::Cylinder()
        : base(Math::Point3D(0, 0, 0)), axis(Math::Vector3D(0, 1, 0)), radius(1.0f), height(1.0f), color(Math::Color(255, 255, 255)) {
    }

    void Cylinder::translate(const Math::Vector3D &translation) {
        base = Math::Point3D(base.getX() + translation.getX(),
                             base.getY() + translation.getY(),
                             base.getZ() + translation.getZ());
    }

    void Cylinder::rotate(float angle, const Math::Vector3D &rotAxis) {
        // Convertir angle en radians
        float radians = angle * M_PI / 180.0f;
        float cosA = cos(radians);
        float sinA = sin(radians);
        
        // Normaliser l'axe de rotation
        float axisLength = sqrt(rotAxis.getX() * rotAxis.getX() + 
                               rotAxis.getY() * rotAxis.getY() + 
                               rotAxis.getZ() * rotAxis.getZ());
        
        float nx = rotAxis.getX() / axisLength;
        float ny = rotAxis.getY() / axisLength;
        float nz = rotAxis.getZ() / axisLength;
        
        // Formule de rotation autour d'un axe arbitraire (Rodrigues)
        // v_rot = v * cos(angle) + (axis × v) * sin(angle) + axis * (axis · v) * (1 - cos(angle))
        
        // Appliquer à l'axe du cylindre
        float dotProduct = nx * axis.getX() + ny * axis.getY() + nz * axis.getZ();
        
        // axisXv = axis × v (produit vectoriel)
        float crossX = ny * axis.getZ() - nz * axis.getY();
        float crossY = nz * axis.getX() - nx * axis.getZ();
        float crossZ = nx * axis.getY() - ny * axis.getX();
        
        // Calculer le nouvel axe du cylindre
        float newAxisX = axis.getX() * cosA + crossX * sinA + nx * dotProduct * (1 - cosA);
        float newAxisY = axis.getY() * cosA + crossY * sinA + ny * dotProduct * (1 - cosA);
        float newAxisZ = axis.getZ() * cosA + crossZ * sinA + nz * dotProduct * (1 - cosA);
        
        // Mise à jour de l'axe du cylindre
        axis = Math::Vector3D(newAxisX, newAxisY, newAxisZ);
        
        // Pour un vrai cylindre couché, il faut aussi:
        // 1. Ajuster l'algorithme d'intersection pour supporter un axe arbitraire
        std::cout << "Cylinder axis after rotation: (" << axis.getX() << ", " 
                  << axis.getY() << ", " << axis.getZ() << ")" << std::endl;
    }

    bool Cylinder::hits(const Ray &ray) const {
        HitRecord rec;
        return intersect(ray, rec);
    }

    // Intersection rayon/cylindre infini (axe Y), borné en hauteur + bases
    bool Cylinder::intersect(const Ray &ray, HitRecord &rec) const {
        // Vecteur d'axe normalisé
        float axisLength = sqrt(axis.getX() * axis.getX() + axis.getY() * axis.getY() + axis.getZ() * axis.getZ());
        float axisX = axis.getX() / axisLength;
        float axisY = axis.getY() / axisLength;
        float axisZ = axis.getZ() / axisLength;
        
        // Direction et origine du rayon
        float dx = ray.getDirection().getX();
        float dy = ray.getDirection().getY();
        float dz = ray.getDirection().getZ();
        
        // Origine relative à la base du cylindre
        float ox = ray.getOrigin().getX() - base.getX();
        float oy = ray.getOrigin().getY() - base.getY();
        float oz = ray.getOrigin().getZ() - base.getZ();
        
        // Produit scalaire dir·axis
        float dirDotAxis = dx * axisX + dy * axisY + dz * axisZ;
        
        // Rejeter la composante parallèle à l'axe
        float dPerpX = dx - dirDotAxis * axisX;
        float dPerpY = dy - dirDotAxis * axisY;
        float dPerpZ = dz - dirDotAxis * axisZ;
        
        // Origine - projection sur l'axe
        float originDotAxis = ox * axisX + oy * axisY + oz * axisZ;
        float oPerpX = ox - originDotAxis * axisX;
        float oPerpY = oy - originDotAxis * axisY;
        float oPerpZ = oz - originDotAxis * axisZ;
        
        // Coefficients de l'équation quadratique
        float a = dPerpX * dPerpX + dPerpY * dPerpY + dPerpZ * dPerpZ;
        float b = 2.0 * (oPerpX * dPerpX + oPerpY * dPerpY + oPerpZ * dPerpZ);
        float c = oPerpX * oPerpX + oPerpY * oPerpY + oPerpZ * oPerpZ - radius * radius;
        
        // Résoudre l'équation quadratique
        float disc = b * b - 4 * a * c;
        float t_side = INFINITY;
        bool hit_side = false;
        
        if (disc >= 0 && a > 1e-6) {
            float sqrtDisc = sqrt(disc);
            float t1 = (-b - sqrtDisc) / (2 * a);
            float t2 = (-b + sqrtDisc) / (2 * a);
            
            // Position du point d'intersection
            if (t1 > 0.001) {
                float hitX = ray.getOrigin().getX() + t1 * dx;
                float hitY = ray.getOrigin().getY() + t1 * dy;
                float hitZ = ray.getOrigin().getZ() + t1 * dz;
                
                // Vecteur de la base au point d'impact
                float vx = hitX - base.getX();
                float vy = hitY - base.getY();
                float vz = hitZ - base.getZ();
                
                // Projection sur l'axe
                float proj = vx * axisX + vy * axisY + vz * axisZ;
                
                // Vérifier si le point est dans les limites de hauteur
                if (proj >= 0 && proj <= height) {
                    t_side = t1;
                    hit_side = true;
                }
            }
            
            if (t2 > 0.001) {
                float hitX = ray.getOrigin().getX() + t2 * dx;
                float hitY = ray.getOrigin().getY() + t2 * dy;
                float hitZ = ray.getOrigin().getZ() + t2 * dz;
                
                // Vecteur de la base au point d'impact
                float vx = hitX - base.getX();
                float vy = hitY - base.getY();
                float vz = hitZ - base.getZ();
                
                // Projection sur l'axe
                float proj = vx * axisX + vy * axisY + vz * axisZ;
                
                // Vérifier si le point est dans les limites de hauteur et plus proche
                if (proj >= 0 && proj <= height && t2 < t_side) {
                    t_side = t2;
                    hit_side = true;
                }
            }
        }
        
        // Intersection avec les bases (disques aux extrémités)
        float t_base = INFINITY;
        bool hit_base = false;
        int base_index = -1; // 0 pour base inférieure, 1 pour base supérieure
        
        // Vecteur vers le second disque
        float topX = base.getX() + height * axisX;
        float topY = base.getY() + height * axisY;
        float topZ = base.getZ() + height * axisZ;
        
        // Intersection avec le disque inférieur
        if (dirDotAxis != 0) {
            float t = -(originDotAxis) / dirDotAxis;
            if (t > 0.001) {
                float hitX = ray.getOrigin().getX() + t * dx;
                float hitY = ray.getOrigin().getY() + t * dy;
                float hitZ = ray.getOrigin().getZ() + t * dz;
                
                // Distance au centre du disque
                float distX = hitX - base.getX();
                float distY = hitY - base.getY();
                float distZ = hitZ - base.getZ();
                
                // Rejeter la composante parallèle à l'axe
                float projDist = distX * axisX + distY * axisY + distZ * axisZ;
                float perpX = distX - projDist * axisX;
                float perpY = distY - projDist * axisY;
                float perpZ = distZ - projDist * axisZ;
                
                float dist2 = perpX * perpX + perpY * perpY + perpZ * perpZ;
                
                if (dist2 <= radius * radius) {
                    t_base = t;
                    hit_base = true;
                    base_index = 0;
                }
            }
        }
        
        // Intersection avec le disque supérieur
        if (dirDotAxis != 0) {
            float t = (height - originDotAxis) / dirDotAxis;
            if (t > 0.001 && t < t_base) {
                float hitX = ray.getOrigin().getX() + t * dx;
                float hitY = ray.getOrigin().getY() + t * dy;
                float hitZ = ray.getOrigin().getZ() + t * dz;
                
                // Distance au centre du disque supérieur
                float distX = hitX - topX;
                float distY = hitY - topY;
                float distZ = hitZ - topZ;
                
                // Rejeter la composante parallèle à l'axe
                float projDist = distX * axisX + distY * axisY + distZ * axisZ;
                float perpX = distX - projDist * axisX;
                float perpY = distY - projDist * axisY;
                float perpZ = distZ - projDist * axisZ;
                
                float dist2 = perpX * perpX + perpY * perpY + perpZ * perpZ;
                
                if (dist2 <= radius * radius) {
                    t_base = t;
                    hit_base = true;
                    base_index = 1;
                }
            }
        }
        
        // Sélectionner l'intersection la plus proche
        if (!hit_side && !hit_base) return false;
        
        if (hit_side && (!hit_base || t_side < t_base)) {
            // Intersection avec la surface latérale
            rec.t = t_side;
            rec.point = Math::Point3D(
                ray.getOrigin().getX() + t_side * dx,
                ray.getOrigin().getY() + t_side * dy,
                ray.getOrigin().getZ() + t_side * dz
            );
            
            // Calculer la normale
            float hitX = rec.point.getX() - base.getX();
            float hitY = rec.point.getY() - base.getY();
            float hitZ = rec.point.getZ() - base.getZ();
            
            // Projection sur l'axe
            float proj = hitX * axisX + hitY * axisY + hitZ * axisZ;
            
            // Point sur l'axe
            float axisPointX = base.getX() + proj * axisX;
            float axisPointY = base.getY() + proj * axisY;
            float axisPointZ = base.getZ() + proj * axisZ;
            
            // Vecteur normal (de l'axe vers le point d'impact)
            float normalX = rec.point.getX() - axisPointX;
            float normalY = rec.point.getY() - axisPointY;
            float normalZ = rec.point.getZ() - axisPointZ;
            
            // Normaliser
            float normalLength = sqrt(normalX * normalX + normalY * normalY + normalZ * normalZ);
            rec.normal = Math::Vector3D(
                normalX / normalLength,
                normalY / normalLength,
                normalZ / normalLength
            );
            
            rec.color = color;
            return true;
        } else {
            // Intersection avec une base
            rec.t = t_base;
            rec.point = Math::Point3D(
                ray.getOrigin().getX() + t_base * dx,
                ray.getOrigin().getY() + t_base * dy,
                ray.getOrigin().getZ() + t_base * dz
            );
            
            // Normale (direction de l'axe ou opposée selon la base)
            if (base_index == 0) {
                rec.normal = Math::Vector3D(-axisX, -axisY, -axisZ);
            } else {
                rec.normal = Math::Vector3D(axisX, axisY, axisZ);
            }
            
            rec.color = color;
            return true;
        }
    }

    extern "C" IPlugin *createCylinderPrimitive() {
        return new Cylinder();
    }
}
