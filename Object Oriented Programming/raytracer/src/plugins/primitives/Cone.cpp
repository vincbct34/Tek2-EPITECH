/*
** EPITECH PROJECT, 2025
** RayTracer
** File description:
** Cone
*/

#include "Cone.hpp"

namespace RayTracer {
    Cone::Cone() : vertex(0.0f, 0.0f, 0.0f), axis(0.0f, 1.0f, 0.0f), color(1.0f, 1.0f, 1.0f) {
        // Normaliser l'axe
        float axisLength = std::sqrt(axis.getX()*axis.getX() + axis.getY()*axis.getY() + axis.getZ()*axis.getZ());
        if (axisLength > 0.0001f) {
            this->axis = Math::Vector3D(
                axis.getX() / axisLength,
                axis.getY() / axisLength,
                axis.getZ() / axisLength
            );
        }
    }

    void Cone::translate(const Math::Vector3D &translation) {
        vertex = Math::Point3D(
            vertex.getX() + translation.getX(),
            vertex.getY() + translation.getY(),
            vertex.getZ() + translation.getZ()
        );
    }

    void Cone::rotate(float angle, const Math::Vector3D &rotAxis) {
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
        
        // Formule de rotation de Rodrigues pour l'axe du cône
        // v_rot = v * cos(angle) + (axis × v) * sin(angle) + axis * (axis · v) * (1 - cos(angle))
        
        // Calculer le produit scalaire
        float dotProduct = nx * axis.getX() + ny * axis.getY() + nz * axis.getZ();
        
        // Calculer le produit vectoriel
        float crossX = ny * axis.getZ() - nz * axis.getY();
        float crossY = nz * axis.getX() - nx * axis.getZ();
        float crossZ = nx * axis.getY() - ny * axis.getX();
        
        // Calculer le nouvel axe
        float newAxisX = axis.getX() * cosA + crossX * sinA + nx * dotProduct * (1 - cosA);
        float newAxisY = axis.getY() * cosA + crossY * sinA + ny * dotProduct * (1 - cosA);
        float newAxisZ = axis.getZ() * cosA + crossZ * sinA + nz * dotProduct * (1 - cosA);
        
        // Mettre à jour l'axe du cône
        axis = Math::Vector3D(newAxisX, newAxisY, newAxisZ);
        
        std::cout << "Cone axis after rotation: (" << axis.getX() << ", " 
                  << axis.getY() << ", " << axis.getZ() << ")" << std::endl;
    }

    bool Cone::hits(const Ray &ray) const {
        HitRecord rec;
        return intersect(ray, rec);
    }

    bool Cone::intersect(const Ray &ray, HitRecord &rec) const {
        // Calculer le point à la base du cône (le sommet étant 'vertex')
        Math::Point3D baseCenter(
            vertex.getX() + height * axis.getX(),
            vertex.getY() + height * axis.getY(),
            vertex.getZ() + height * axis.getZ()
        );

        // Direction du rayon
        float dx = ray.getDirection().getX();
        float dy = ray.getDirection().getY();
        float dz = ray.getDirection().getZ();
        
        // Origine du rayon relative au sommet
        float ox = ray.getOrigin().getX() - vertex.getX();
        float oy = ray.getOrigin().getY() - vertex.getY();
        float oz = ray.getOrigin().getZ() - vertex.getZ();
        
        // Composantes de l'axe du cône
        float axisX = axis.getX();
        float axisY = axis.getY();
        float axisZ = axis.getZ();
        
        // Calculer le cosinus de l'angle d'ouverture du cône
        float cosAngle = height / sqrt(height*height + radius*radius);
        float cosAngleSquared = cosAngle * cosAngle;
        
        // Calcul des produits scalaires
        float dotVD = axisX * dx + axisY * dy + axisZ * dz; // Axe · DirectionRayon
        float dotVC = axisX * ox + axisY * oy + axisZ * oz; // Axe · (OrigineRayon - Sommet)
        
        // Calcul des composants de l'équation quadratique
        float a = dotVD * dotVD - cosAngleSquared * (dx * dx + dy * dy + dz * dz);
        float b = 2.0f * (dotVD * dotVC - cosAngleSquared * (ox * dx + oy * dy + oz * dz));
        float c = dotVC * dotVC - cosAngleSquared * (ox * ox + oy * oy + oz * oz);
        
        // Résoudre l'équation quadratique pour la surface latérale
        float discriminant = b * b - 4 * a * c;
        float t_side = INFINITY;
        bool hit_side = false;
        
        if (discriminant >= 0) {
            float sqrtDisc = sqrt(discriminant);
            float t1 = (-b - sqrtDisc) / (2 * a);
            float t2 = (-b + sqrtDisc) / (2 * a);
            
            // Vérifier t1
            if (t1 > 0.001f) {
                // Point d'intersection
                float hitX = ray.getOrigin().getX() + t1 * dx;
                float hitY = ray.getOrigin().getY() + t1 * dy;
                float hitZ = ray.getOrigin().getZ() + t1 * dz;
                
                // Vecteur du sommet au point d'intersection
                float vpX = hitX - vertex.getX();
                float vpY = hitY - vertex.getY();
                float vpZ = hitZ - vertex.getZ();
                
                // Projection sur l'axe
                float projLen = vpX * axisX + vpY * axisY + vpZ * axisZ;
                
                // Vérifier si dans les limites du cône
                if (projLen > 0 && projLen < height) {
                    t_side = t1;
                    hit_side = true;
                }
            }
            
            // Vérifier t2 si nécessaire
            if (t2 > 0.001f && (!hit_side || t2 < t_side)) {
                // Point d'intersection
                float hitX = ray.getOrigin().getX() + t2 * dx;
                float hitY = ray.getOrigin().getY() + t2 * dy;
                float hitZ = ray.getOrigin().getZ() + t2 * dz;
                
                // Vecteur du sommet au point d'intersection
                float vpX = hitX - vertex.getX();
                float vpY = hitY - vertex.getY();
                float vpZ = hitZ - vertex.getZ();
                
                // Projection sur l'axe
                float projLen = vpX * axisX + vpY * axisY + vpZ * axisZ;
                
                // Vérifier si dans les limites du cône
                if (projLen > 0 && projLen < height) {
                    t_side = t2;
                    hit_side = true;
                }
            }
        }
        
        // Vérifier l'intersection avec la base du cône (disque)
        float t_base = INFINITY;
        bool hit_base = false;
        
        // Direction de la normale de la base (opposée à l'axe)
        float baseNormalX = -axisX;
        float baseNormalY = -axisY;
        float baseNormalZ = -axisZ;
        
        // Produit scalaire entre la direction du rayon et la normale de la base
        float dotDirNormal = dx * baseNormalX + dy * baseNormalY + dz * baseNormalZ;
        
        // Vérifier si le rayon n'est pas parallèle à la base
        if (std::abs(dotDirNormal) > 1e-6) {
            // Vecteur de l'origine du rayon au centre de la base
            float ocX = ray.getOrigin().getX() - baseCenter.getX();
            float ocY = ray.getOrigin().getY() - baseCenter.getY();
            float ocZ = ray.getOrigin().getZ() - baseCenter.getZ();
            
            // Calcul du paramètre t pour l'intersection avec le plan de la base
            float t = -(ocX * baseNormalX + ocY * baseNormalY + ocZ * baseNormalZ) / dotDirNormal;
            
            if (t > 0.001f) {
                // Point d'intersection avec le plan
                float hitX = ray.getOrigin().getX() + t * dx;
                float hitY = ray.getOrigin().getY() + t * dy;
                float hitZ = ray.getOrigin().getZ() + t * dz;
                
                // Distance du point au centre de la base
                float hpX = hitX - baseCenter.getX();
                float hpY = hitY - baseCenter.getY();
                float hpZ = hitZ - baseCenter.getZ();
                
                // Carré de la distance
                float dist2 = hpX * hpX + hpY * hpY + hpZ * hpZ;
                
                // Vérifier si le point est dans le disque de la base
                if (dist2 <= radius * radius) {
                    t_base = t;
                    hit_base = true;
                }
            }
        }
        
        // Déterminer l'intersection la plus proche
        if (!hit_side && !hit_base) return false;
        
        if (hit_side && (!hit_base || t_side < t_base)) {
            // Intersection avec la surface latérale
            rec.t = t_side;
            rec.point = Math::Point3D(
                ray.getOrigin().getX() + t_side * dx,
                ray.getOrigin().getY() + t_side * dy,
                ray.getOrigin().getZ() + t_side * dz
            );
            
            // Calcul de la normale au point d'intersection
            
            // Vecteur du sommet au point d'intersection
            float vpX = rec.point.getX() - vertex.getX();
            float vpY = rec.point.getY() - vertex.getY();
            float vpZ = rec.point.getZ() - vertex.getZ();
            
            // Projection sur l'axe
            float projLen = vpX * axisX + vpY * axisY + vpZ * axisZ;
            
            // Point sur l'axe à la même hauteur que le point d'intersection
            float pointOnAxisX = vertex.getX() + projLen * axisX;
            float pointOnAxisY = vertex.getY() + projLen * axisY;
            float pointOnAxisZ = vertex.getZ() + projLen * axisZ;
            
            // Vecteur du point sur l'axe au point d'intersection
            float dirX = rec.point.getX() - pointOnAxisX;
            float dirY = rec.point.getY() - pointOnAxisY;
            float dirZ = rec.point.getZ() - pointOnAxisZ;
            
            // Vecteur pointant du sommet au point d'intersection
            float vhX = rec.point.getX() - vertex.getX();
            float vhY = rec.point.getY() - vertex.getY();
            float vhZ = rec.point.getZ() - vertex.getZ();
            
            // Calculer la normale comme le vecteur perpendiculaire à la fois à l'axe et à dirPoint
            float normalX = dirX * projLen - vhX * cosAngleSquared;
            float normalY = dirY * projLen - vhY * cosAngleSquared;
            float normalZ = dirZ * projLen - vhZ * cosAngleSquared;
            
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
            // Intersection avec la base
            rec.t = t_base;
            rec.point = Math::Point3D(
                ray.getOrigin().getX() + t_base * dx,
                ray.getOrigin().getY() + t_base * dy,
                ray.getOrigin().getZ() + t_base * dz
            );
            
            // La normale est l'opposé de l'axe du cône
            rec.normal = Math::Vector3D(baseNormalX, baseNormalY, baseNormalZ);
            rec.color = color;
            return true;
        }
    }

    extern "C" IPlugin *createConePrimitive() {
        return new Cone();
    }
}
