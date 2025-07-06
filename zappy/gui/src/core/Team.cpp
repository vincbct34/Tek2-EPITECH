/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** team
*/

#include "core/Team.hpp"
#include <iostream>

Team::Team(const std::string &name) : _name(name) {
    static int id = 0;
    teamId = id++;
    switch (teamId) {
        case 0:
            teamColor = RED; // Default color for team 0
            break;
        case 1:
            teamColor = GREEN; // Default color for team 1
            break;
        case 2:
            teamColor = BLUE; // Default color for team 2
            break;
        case 3:
            teamColor = YELLOW; // Default color for team 3
            break;
        default:
            teamColor = WHITE; // Default color for any other team
            break;
    }
    return;
}

Team::~Team() { return; }

size_t Team::getPlayerCountOnTile(int x, int y) const {
    size_t count = 0;

    for (const auto &player : _players) {
        if (player->getPosX() == x && player->getPosY() == y) {
            count++;
        }
        for (const auto &egg : player->getEggs()) {
            if (egg->getPosX() == x && egg->getPosY() == y) {
                count++;
            }
        }
    }
    return count;
}
