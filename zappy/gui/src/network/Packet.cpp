/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Packet
*/

#include "network/Packet.hpp"

Packet Packet::parse(const std::string &raw) {
  Packet pkt;

  std::string arg;
  std::istringstream ss(raw);
  std::getline(ss, pkt.type, ':');
  while (std::getline(ss, arg, ':'))
    pkt.args.push_back(arg);
  return pkt;
}

std::string Packet::serialize() const {
  std::ostringstream ss;

  ss << type;
  for (const auto &arg : args)
    ss << ':' << arg;

  return ss.str();
}
