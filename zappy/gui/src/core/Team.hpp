/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Team
*/

#pragma once

#include "core/Game.hpp"
#include "core/Player.hpp"
#include "utils/CustomRayLib.hpp"

#include <string>
#include <vector>
#include <memory>
#include <iostream>

class Player;

class Team {
public:
  Team(const std::string &name);
  ~Team();

  /**
   * @brief Gets the name of the team.
   *
   * @return The name of the team.
   */
  std::string getName() const { return _name; }

  /**
   * @brief Gets the unique identifier of the team.
   *
   * @return The unique identifier of the team.
   */
  int getId() const { return teamId; }

  /**
   * @brief Gets the color of the team.
   *
   * @return The color of the team.
   */
  Color getColor() const { return teamColor; }

  void addPlayer(const Player &player) {
    _players.push_back(std::make_unique<Player>(player));
  }
  /**
   * @brief Gets the list of players in the team.
   *
   * @return A vector of unique pointers to players in the team.
   */
  std::vector<std::unique_ptr<Player>> &getPlayers() { return _players; }
  /**
   * @brief Gets the number of players in the team.
   *
   * @return The number of players in the team.
   */
  size_t getPlayerCount() const { return _players.size(); }

  /**
   * @brief Gets the number of players on a specific tile.
   *
   * @param x The x-coordinate of the tile.
   * @param y The y-coordinate of the tile.
   * @return The number of players on the specified tile.
   */
  size_t getPlayerCountOnTile(int x, int y) const;
private:
  std::string _name;
  std::vector<std::unique_ptr<Player>> _players;
  int teamId = 0;
  Color teamColor = WHITE;
};
