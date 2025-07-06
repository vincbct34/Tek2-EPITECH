/*
** EPITECH PROJECT, 2025
** Interstonar [WSL: Ubuntu-24.04]
** File description:
** Body
*/

#include "Body.hpp"

Body::Body()
    : name(""),
      position(Vector3()),
      velocity(Vector3()),
      mass(0.0),
      radius(0.0),
      isGoal(false),
      type(UNKNOWN),
      height(0.0),
      boxSize(Vector3()),
      innerRadius(0.0),
      outerRadius(0.0)
{}

void Body::printLocalInfos() const
{
	std::cout << type << " ";

	switch (type) {
		case SPHERE:
			std::cout << "of radius " << radius << " at position ("
				<< position.x << ", " << position.y << ", " << position.z << ")"
				<< std::endl;
			break;
		case CYLINDER:
			std::cout << "of radius " << radius << " and height " << height
				<< " at position (" << position.x << ", " << position.y << ", " << position.z << ")"
				<< std::endl;
			break;
		case BOX:
			std::cout << "of dimensions (" << boxSize.x << ", " << boxSize.y << ", " << boxSize.z << ")"
				<< " at position (" << position.x << ", " << position.y << ", " << position.z << ")"
				<< std::endl;
			break;
		case TORUS:
			std::cout << "of inner radius " << innerRadius << " and outer radius " << outerRadius
				<< " at position (" << position.x << ", " << position.y << ", " << position.z << ")"
				<< std::endl;
			break;
		default:
			std::cout << "Unknown type" << std::endl;
			break;
	}
}
