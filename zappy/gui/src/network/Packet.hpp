/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Packet
*/

#pragma once

#include <sstream>
#include <string>
#include <vector>

struct Packet {
  std::string type;              // Instruction type of the packet
  std::vector<std::string> args; // Arguments of the packet

  /**
   * @brief Parses a raw string into a Packet.
   *
   * @param raw The raw string to parse.
   * @return The parsed Packet.
   */
  static Packet parse(const std::string &raw);
  /**
   * @brief Serializes the Packet into <TO DETERMINE> format.
   *
   * @return The serialized string representation of the Packet.
   */
  std::string serialize() const;
};
