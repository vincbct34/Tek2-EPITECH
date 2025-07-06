/*
** EPITECH PROJECT, 2025
** Interstonar [WSL: Ubuntu-24.04]
** File description:
** Body
*/

#ifndef BODY_HPP_
    #define BODY_HPP_

#include "Vector3.hpp"
#include <string>

enum BodyType {
    UNKNOWN,
    CYLINDER,
    SPHERE,
    TORUS,
    BOX
};

class Body
{
    public:
        Body();

        void printLocalInfos() const;

        std::string name;
        
        Vector3 position;
        Vector3 velocity;
        
        double mass;
        double radius;
        
        bool isGoal;

        BodyType type;

        // Pour le local mode :
        double height; // pour les cylindres
        Vector3 boxSize; // pour les boxes
        double innerRadius, outerRadius; // pour les tores
};

inline std::ostream &operator<<(std::ostream &os, const BodyType &type)
{
    switch (type) {
        case UNKNOWN: os << "Unknown"; break;
        case CYLINDER: os << "Cylinder"; break;
        case SPHERE: os << "Sphere"; break;
        case TORUS: os << "Torus"; break;
        case BOX: os << "Box"; break;
        default: os << "INVALID"; break;
    }
    return os;
}

#endif /* !BODY_HPP_ */
