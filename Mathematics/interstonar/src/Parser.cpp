/*
** EPITECH PROJECT, 2025
** Interstonar [WSL: Ubuntu-24.04]
** File description:
** Parser
*/

#include "Parser.hpp"
#include "Body.hpp"
#include "Exceptions.hpp"

BodyType parseType(const std::string& typeStr) {
    // Get body type, for local mode
    if (typeStr == "sphere")
        return SPHERE;

    if (typeStr == "cylinder")
        return CYLINDER;

    if (typeStr == "box")
        return BOX;

    if (typeStr == "torus")
        return TORUS;

    return UNKNOWN;
}

Vector3 parseVector(const std::shared_ptr<cpptoml::table>& b, const std::string& prefix) {
    auto x = b->get_qualified_as<double>(prefix + ".x");
    auto y = b->get_qualified_as<double>(prefix + ".y");
    auto z = b->get_qualified_as<double>(prefix + ".z");

    return Vector3(*x, *y, *z); // Return the vector found using the cpptoml lib
}

std::vector<Body> parseConfig(const std::string& filename) {
    std::vector<Body> bodies;

    // Booleans to check if the bodies have a position and a velocity after parsing
    bool hasPosition = false;
    bool hasVelocity = false;

    auto config = cpptoml::parse_file(filename);
    auto bodyArray = config->get_table_array("bodies");

    if (!bodyArray)
        throw ParseException("No bodies found in the configuration file");

    for (auto& b : *bodyArray) {
        Body body;

        if (auto name = b->get_as<std::string>("name"))
            body.name = *name;

        if (b->contains("position")) {
            hasPosition = true;
            body.position = parseVector(b, "position");
        }

        if (b->contains("direction")) {
            hasVelocity = true;
            body.velocity = parseVector(b, "direction");
        }

        if (auto mass = b->get_as<double>("mass"))
            body.mass = *mass;

        if (auto radius = b->get_as<double>("radius"))
            body.radius = *radius;

        if (auto goal = b->get_as<bool>("goal"))
            body.isGoal = *goal;

        if (auto type = b->get_as<std::string>("type"))
            body.type = parseType(*type);

        if (auto height = b->get_as<double>("height"))
            body.height = *height;

        if (b->contains("sides"))
            body.boxSize = parseVector(b, "sides");

        if (auto r1 = b->get_as<double>("inner_radius"))
            body.innerRadius = *r1;

        if (auto r2 = b->get_as<double>("outer_radius"))
            body.outerRadius = *r2;

        if (body.name.empty() || body.mass == 0 || !hasPosition || !hasVelocity)
            throw ParseException("Missing required fields in body configuration");

        bodies.push_back(body);
    }

    return bodies;
}

std::vector<Body> parseLocalConfig(const std::string& filename)
{

    std::vector<Body> bodies;

    bool hasPosition = false;

    auto config = cpptoml::parse_file(filename);
    auto bodyArray = config->get_table_array("bodies");

    if (!bodyArray)
        throw ParseException("No bodies found in the configuration file");

    for (auto& b : *bodyArray) {
        Body body;

        if (b->contains("position")) {
            hasPosition = true;
            body.position = parseVector(b, "position");
        }

        if (auto mass = b->get_as<double>("mass"))
            body.mass = *mass;

        if (auto radius = b->get_as<double>("radius"))
            body.radius = *radius;

        if (auto goal = b->get_as<bool>("goal"))
            body.isGoal = *goal;

        if (auto type = b->get_as<std::string>("type"))
            body.type = parseType(*type);

        if (auto height = b->get_as<double>("height"))
            body.height = *height;

        if (b->contains("sides"))
            body.boxSize = parseVector(b, "sides");

        if (auto r1 = b->get_as<double>("inner_radius"))
            body.innerRadius = *r1;

        if (auto r2 = b->get_as<double>("outer_radius"))
            body.outerRadius = *r2;

        if (!hasPosition)
            throw ParseException("Missing required fields in body configuration");

        bodies.push_back(body);
    }

    return bodies;
}
