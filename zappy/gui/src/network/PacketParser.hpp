/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** PacketParser
*/

#pragma once

#include "NetworkClient.hpp"
#include <string>
#include <vector>
#include <sstream>
#include <unordered_map>
#include <functional>

class Game;

class PacketParser {
public:
    PacketParser(Game& game, NetworkClient& networkClient);
    void parseAndExecute(const std::string& packet);
    void initializeCommands();

private:
    Game& game;
    NetworkClient& networkClient;
    std::unordered_map<std::string, std::function<void(const std::vector<std::string>&)>> commandFactory;

    std::vector<std::string> split(const std::string& str, char delimiter);
    void handleWelcome(const std::vector<std::string>& args);
    void handleMsz(const std::vector<std::string>& args);
    void handleBct(const std::vector<std::string>& args);
    void handleTna(const std::vector<std::string>& args);
    void handlePnw(const std::vector<std::string>& args);
    void handlePpo(const std::vector<std::string>& args);
    void handlePlv(const std::vector<std::string>& args);
    void handlePin(const std::vector<std::string>& args);
    void handlePdi(const std::vector<std::string>& args);
    void handleEnw(const std::vector<std::string>& args);
    void handleEht(const std::vector<std::string>& args);
    void handleEbo(const std::vector<std::string>& args);
    void handleEdi(const std::vector<std::string>& args);
    void handlePic(const std::vector<std::string>& args);
    void handlePie(const std::vector<std::string>& args);
    void handleSeg(const std::vector<std::string>& args);
    void handlePgt(const std::vector<std::string>& args);
    void handlePdr(const std::vector<std::string>& args);
    void handleRemres(const std::vector<std::string>& args);
};
