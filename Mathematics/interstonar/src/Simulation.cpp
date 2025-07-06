/*
** EPITECH PROJECT, 2025
** Interstonar [WSL: Ubuntu-24.04]
** File description:
** Simulation
*/

#include "Simulation.hpp"

bool checkCollision(const Body& a, const Body& b) {
    return (a.position - b.position).norm() <= (a.radius + b.radius); // Check if the distance between the two bodies is less than or equal to the sum of their radius
}

Vector3 computeAcceleration(const Vector3& pos, const std::vector<Body>& bodies) {
    Vector3 total;

    for (const auto& body : bodies) {
        Vector3 r = body.position - pos; // Vector from the first body to the second body
        double d = r.norm(); // Distance between the two bodies

        if (d == 0.0)
            continue;

        double a = GRAVIT_VAR * body.mass / (d * d); // Gravitational acceleration
        total = total + r.normalize() * a; // Normalize the vector and scale it by the acceleration
    }

    return total;
}

void mergeBodies(Body& firstBody, const Body& secondBody) {
    double totalMass = firstBody.mass + secondBody.mass; // Get total mass from both bodies

    double volumeFirst = (4.0 / 3.0) * M_PI * std::pow(firstBody.radius, 3);
    double volumeSecond = (4.0 / 3.0) * M_PI * std::pow(secondBody.radius, 3);
    double combinedVolume = volumeFirst + volumeSecond;
    double newRadius = std::cbrt((3.0 * combinedVolume) / (4.0 * M_PI)); // Get new radius from combined volume

    Vector3 newPosition = (firstBody.position + secondBody.position) / 2.0; // Get new position as average of both bodies

    Vector3 newVelocity = (firstBody.velocity * firstBody.mass + secondBody.velocity * secondBody.mass) / totalMass; // Get new velocity as weighted average of both bodies

    std::string firstName = firstBody.name;
    std::string secondName = secondBody.name;
    std::string mergedName = firstName < secondName ? firstName + secondName : secondName + firstName; // Get new name as concatenation of both names sorting alphabetically

    bool newGoal = firstBody.isGoal || secondBody.isGoal; // Get new goal status as OR of both bodies

    firstBody.mass = totalMass;
    firstBody.radius = newRadius;
    firstBody.position = newPosition;
    firstBody.velocity = newVelocity;
    firstBody.name = mergedName;
    firstBody.isGoal = newGoal;
}

void simulateGlobal(const std::string& configPath, const Vector3& initialPosition, const Vector3& initialVelocity) {
    std::vector<Body> bodies = parseConfig(configPath); // Parse the configuration file to get the celestial bodies

    Body rock; // Create the rock body
    rock.name = "rock";
    rock.mass = 1;
    rock.radius = 0;
    rock.position = initialPosition;
    rock.velocity = initialVelocity;

    std::cout << std::fixed << std::setprecision(3);

    for (int t = 1; t <= MAX_STEPS; ++t) {
        std::vector<size_t> toRemove; // List of the bodies that have merged and that have to be removed, reinitialized every loop
    
        // 1. Check collision with the rock
        for (const auto& body : bodies) {
            if (checkCollision(rock, body)) {
                std::cout << "Collision between rock and " << body.name << std::endl;
                std::cout << (body.isGoal ? "Mission success" : "Mission failure") << std::endl;
                return;
            }
        }
    
        // 2. Compute accelerations
        Vector3 accel = computeAcceleration(rock.position, bodies);
        for (auto& body : bodies) {
            Vector3 acc = computeAcceleration(body.position, bodies);
            body.velocity = body.velocity + acc * DELTA_T;
        }
    
        // 3. Update the positions of all bodies
        for (auto& body : bodies) {
            body.position = body.position + body.velocity * DELTA_T;
        }
        rock.position = rock.position + rock.velocity * DELTA_T;

        // 4. Update the velocity of the rock
        rock.velocity = rock.velocity + accel * DELTA_T;

        // 5. Display the position of the rock
        std::cout << "At time t = " << t << ": rock is ("
                  << rock.position.x << ", " << rock.position.y << ", " << rock.position.z << ")" << std::endl;
    
        // 6. Merge bodies in collision
        for (size_t i = 0; i < bodies.size(); ++i) {
            for (size_t j = i + 1; j < bodies.size(); ) { // Use a while loop for the inner loop
                if (checkCollision(bodies[i], bodies[j])) {
                    mergeBodies(bodies[i], bodies[j]);
                    bodies.erase(bodies.begin() + j); // Erase the element
                } else {
                    ++j; // Only increment j if no element was erased
                }
            }
        }
    }

    std::cout << "Mission failure" << std::endl; // If the rock never collides or collides with a body that is not a goal, the program fails
}


double sdfSphere(const Vector3& p, double radius)
{
    return p.length() - radius;
}

double sdfBox(const Vector3& p, const Vector3& size)
{
    Vector3 d = p.abs() - size * 0.5;
    double inside = std::min(std::max(d.x, std::max(d.y, d.z)), 0.0);
    Vector3 outside(
        std::max(d.x, 0.0),
        std::max(d.y, 0.0),
        std::max(d.z, 0.0)
    );
    return inside + outside.length();
}

double sdfCylinder(const Vector3& p, double height, double radius)
{
    // ! le cylindre est aligné sur l'axe Z
    Vector3 q(std::sqrt(p.x * p.x + p.y * p.y), std::abs(p.z), 0.0);
    Vector3 d = q - Vector3(radius, height / 2.0, 0.0);
    double inside = std::min(std::max(d.x, d.z), 0.0);
    Vector3 outside(std::max(d.x, 0.0), std::max(d.z, 0.0), 0.0);
    return inside + outside.length();
}

double sdfTorus(const Vector3& p, double innerRadius, double outerRadius)
{
    Vector3 q(std::sqrt(p.x * p.x + p.z * p.z) - outerRadius, p.y, 0.0);
    return q.length() - innerRadius;
}

double sdfPoint(const Vector3& p)
{
    return p.length();
}

double distanceBetween(const Body &a, const Body &b)
{
    Vector3 p = b.position - a.position;

    double da = 0.0;
    double db = 0.0;

    // std::cout << "[DEBUG] a.type = " << a.type << std::endl; // TODO: remove this line
    // std::cout << "[DEBUG] b.type = " << b.type << std::endl; // TODO: remove this line

    switch (a.type) {
        case SPHERE:
            da = sdfSphere(Vector3(0, 0, 0), a.radius);
            break;
        case CYLINDER:
            da = sdfCylinder(Vector3(0, 0, 0), a.height, a.radius);
            break;
        case BOX:
            da = sdfBox(Vector3(0, 0, 0), a.boxSize);
            break;
        case TORUS:
            da = sdfTorus(Vector3(0, 0, 0), a.innerRadius, a.outerRadius);
            break;
        default:
            da = sdfPoint(Vector3(0, 0, 0));
    }

    switch (b.type) {
        case SPHERE:
            db = sdfSphere(p, b.radius);
            break;
        case CYLINDER:
            db = sdfCylinder(p, b.height, b.radius);
            break;
        case BOX:
            db = sdfBox(p, b.boxSize);
            break;
        case TORUS:
            db = sdfTorus(p, b.innerRadius, b.outerRadius);
            break;
        default:
            return -1.0;
    }

    // std::cout << "[DEBUG] da = " << da << ", db = " << db << std::endl; // TODO: remove this line

    return da + db;
}

double getMinDistance(const Body &rock, const std::vector<Body> &bodies)
{
    double minDistance = std::numeric_limits<double>::max();

    for (const auto& body : bodies) {
        double distance = distanceBetween(rock, body);
        // std::cout << "[DEBUG] distance between rock and " << body.type << " = " << distance << std::endl; // TODO: remove this line
        if (distance < minDistance) {
            minDistance = distance;
        }
    }
    return minDistance;
}

void simulateLocal(const std::string& configPath, const Vector3& position, const Vector3& velocity)
{
    std::vector<Body> bodies = parseLocalConfig(configPath);

    Vector3 rockDirection = velocity.normalize();

    Body rock;
    rock.name = "rock";
    rock.mass = 1;
    rock.radius = 0;
    rock.position = position;
    rock.velocity = velocity;
    rock.isGoal = false;
    rock.type = UNKNOWN;

    std::cout << std::fixed << std::setprecision(2);

    std::cout << "Rock thrown at the point ("
                << rock.position.x << ", " << rock.position.y << ", " << rock.position.z << ")"
                << " and parallel to the vector ("
                << rock.velocity.x << ", " << rock.velocity.y << ", " << rock.velocity.z << ")"
                << std::endl;
    
    for (const auto& body : bodies) {
        body.printLocalInfos();
    }

    std::cout << std::endl;
    
    bool found = false;

    for (int t = 1; t <= 1000; ++t) {

        // std::cout << "[DEBUG] t = " << t << std::endl; // TODO: remove this line
        
        double distance = getMinDistance(rock, bodies);

        // std::cout << "[DEBUG] Min distance = " << distance << std::endl; // TODO: remove this line

        if (distance > 1000) break;

        rock.position.x += (rockDirection.x * distance);
        rock.position.y += (rockDirection.y * distance);
        rock.position.z += (rockDirection.z * distance);
        
        // std::cout << "Min distance: " << distance << std::endl; // TODO: remove this line

        std::cout << "Step " << t << ": ("
            << rock.position.x << ", " << rock.position.y << ", " << rock.position.z << ")"
            << std::endl;
        
        if (distance <= 0.1) {
            found = true;
            break;
        }

        // std::cout << "===============================================" << std::endl; // TODO: remove this line
    }

    std::cout << "Result: " << (found ? "Intersection" : "Out of scene") << std::endl;
}
