/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** PacketParser
*/

#include "PacketParser.hpp"
#include "core/Game.hpp"
#include <iostream>

PacketParser::PacketParser(Game& game, NetworkClient& networkClient) 
    : game(game), networkClient(networkClient) {
    initializeCommands();
}

void PacketParser::initializeCommands() {
    commandFactory["WELCOME"] = [this](const std::vector<std::string>& args) { handleWelcome(args); };
    commandFactory["msz"] = [this](const std::vector<std::string>& args) { handleMsz(args); };
    commandFactory["bct"] = [this](const std::vector<std::string>& args) { handleBct(args); };
    commandFactory["tna"] = [this](const std::vector<std::string>& args) { handleTna(args); };
    commandFactory["pnw"] = [this](const std::vector<std::string>& args) { handlePnw(args); };
    commandFactory["ppo"] = [this](const std::vector<std::string>& args) { handlePpo(args); };
    commandFactory["plv"] = [this](const std::vector<std::string>& args) { handlePlv(args); };
    commandFactory["pin"] = [this](const std::vector<std::string>& args) { handlePin(args); };
    commandFactory["pdi"] = [this](const std::vector<std::string>& args) { handlePdi(args); };
    commandFactory["enw"] = [this](const std::vector<std::string>& args) { handleEnw(args); };
    commandFactory["eht"] = [this](const std::vector<std::string>& args) { handleEht(args); };
    commandFactory["ebo"] = [this](const std::vector<std::string>& args) { handleEbo(args); };
    commandFactory["edi"] = [this](const std::vector<std::string>& args) { handleEdi(args); };
    commandFactory["pic"] = [this](const std::vector<std::string>& args) { handlePic(args); };
    commandFactory["pie"] = [this](const std::vector<std::string>& args) { handlePie(args); };
    commandFactory["seg"] = [this](const std::vector<std::string>& args) { handleSeg(args); };
    commandFactory["pgt"] = [this](const std::vector<std::string>& args) { handlePgt(args); };
    commandFactory["pdr"] = [this](const std::vector<std::string>& args) { handlePdr(args); };
    commandFactory["remres"] = [this](const std::vector<std::string>& args) { handleRemres(args); };
}

std::vector<std::string> PacketParser::split(const std::string& str, char delimiter) {
    std::vector<std::string> tokens;
    std::stringstream ss(str);
    std::string token;
    
    while (std::getline(ss, token, delimiter)) {
        if (!token.empty()) {
            tokens.push_back(token);
        }
    }
    return tokens;
}

void PacketParser::parseAndExecute(const std::string& packet) {
    std::string cleanPacket = packet;
    cleanPacket.erase(cleanPacket.find_last_not_of(" \n\r\t") + 1);

    if (cleanPacket.empty()) return;

    std::vector<std::string> args = split(cleanPacket, ' ');
    if (args.empty()) return;

    std::string command = args[0];

    auto it = commandFactory.find(command);
    if (it != commandFactory.end()) {
        try {
            it->second(args);
        } catch (const std::exception& e) {
            std::cerr << "[ERROR] Error executing command '" << command << "': " << e.what() << std::endl;
        }
    } else {
        std::cout << "[DEBUG] Unknown command: [" << command << "]" << std::endl;
    }
}

void PacketParser::handleWelcome(const std::vector<std::string>&) {
    std::cout << "[DEBUG] Received WELCOME packet - sending GRAPHIC" << std::endl;
    networkClient.sendPacket(Packet{"GRAPHIC", {"GRAPHIC"}});
}

void PacketParser::handleMsz(const std::vector<std::string>& args) {
    if (args.size() < 3) {
        std::cerr << "[ERROR] msz: Invalid number of arguments" << std::endl;
        return;
    }
    
    int x = std::stoi(args[1]);
    int y = std::stoi(args[2]);
    
    std::cout << "[DEBUG] Creating game with size: " << x << "x" << y << std::endl;
    game.initializeGrid(x, y);
}

void PacketParser::handleBct(const std::vector<std::string>& args) {
    if (args.size() < 10) {
        std::cerr << "[ERROR] bct: Invalid number of arguments" << std::endl;
        return;
    }
    
    int x = std::stoi(args[1]);
    int y = std::stoi(args[2]);
    int q0 = std::stoi(args[3]); // food
    int q1 = std::stoi(args[4]); // linemate
    int q2 = std::stoi(args[5]); // deraumere
    int q3 = std::stoi(args[6]); // sibur
    int q4 = std::stoi(args[7]); // mendiane
    int q5 = std::stoi(args[8]); // phiras
    int q6 = std::stoi(args[9]); // thystame
    
    game.fillTileMap(x, y, q0, q1, q2, q3, q4, q5, q6);
}

void PacketParser::handleTna(const std::vector<std::string>& args) {
    if (args.size() < 2) {
        std::cerr << "[ERROR] tna: Invalid number of arguments" << std::endl;
        return;
    }
    
    std::string teamName = args[1];
    game.createTeam(teamName);
}

void PacketParser::handlePnw(const std::vector<std::string>& args) {
    if (args.size() < 7) {
        std::cerr << "[ERROR] pnw: Invalid number of arguments" << std::endl;
        return;
    }
    
    std::string idStr = args[1];
    if (idStr.front() == '#') {
        idStr = idStr.substr(1);
    }
    int id = std::stoi(idStr);
    int x = std::stoi(args[2]);
    int y = std::stoi(args[3]);
    int orientation = std::stoi(args[4]);
    int level = std::stoi(args[5]);
    std::string teamName = args[6];
    
    game.createPlayer(id, x, y, orientation, level, teamName);
}

void PacketParser::handlePpo(const std::vector<std::string>& args) {
    if (args.size() < 5) {
        std::cerr << "[ERROR] ppo: Invalid number of arguments" << std::endl;
        return;
    }
    
    std::string idStr = args[1];
    if (idStr.front() == '#') {
        idStr = idStr.substr(1);
    }
    int id = std::stoi(idStr);
    int x = std::stoi(args[2]);
    int y = std::stoi(args[3]);
    int orientation = std::stoi(args[4]);
    
    game.updatePlayer(id, x, y, orientation, -1);
}

void PacketParser::handlePlv(const std::vector<std::string>& args) {
    if (args.size() < 3) {
        std::cerr << "[ERROR] plv: Invalid number of arguments" << std::endl;
        return;
    }
    
    std::string idStr = args[1];
    if (idStr.front() == '#') {
        idStr = idStr.substr(1);
    }
    int id = std::stoi(idStr);
    int level = std::stoi(args[2]);
    
    game.updatePlayer(id, -1, -1, -1, level);
}

void PacketParser::handlePin(const std::vector<std::string>& args) {
    if (args.size() < 10) {
        std::cerr << "[ERROR] pin: Invalid number of arguments" << std::endl;
        return;
    }
    
    std::string idStr = args[1];
    if (idStr.front() == '#') {
        idStr = idStr.substr(1);
    }
    int id = std::stoi(idStr);
    int q0 = std::stoi(args[2]);
    int q1 = std::stoi(args[3]);
    int q2 = std::stoi(args[4]);
    int q3 = std::stoi(args[5]);
    int q4 = std::stoi(args[6]);
    int q5 = std::stoi(args[7]);
    int q6 = std::stoi(args[8]);

    game.updateInventoryPlayer(id, q0, q1, q2, q3, q4, q5, q6);
}

void PacketParser::handlePdi(const std::vector<std::string>& args) {
    if (args.size() < 2) {
        std::cerr << "[ERROR] pdi: Invalid number of arguments" << std::endl;
        return;
    }
    
    std::string idStr = args[1];
    if (idStr.front() == '#') {
        idStr = idStr.substr(1);
    }
    int id = std::stoi(idStr);
    game.removePlayer(id);
}

void PacketParser::handleEnw(const std::vector<std::string>& args) {
    if (args.size() < 5) {
        std::cerr << "[ERROR] enw: Invalid number of arguments" << std::endl;
        return;
    }
    
    int eggId = std::stoi(args[1]);
    int playerId = std::stoi(args[2]);
    int x = std::stoi(args[3]);
    int y = std::stoi(args[4]);
    
    game.createEgg(playerId, eggId, x, y);
}

void PacketParser::handleEht(const std::vector<std::string>& args) {
    if (args.size() < 2) {
        std::cerr << "[ERROR] eht: Invalid number of arguments" << std::endl;
        return;
    }
    
    std::string idStr = args[1];
    if (idStr.front() == '#') {
        idStr = idStr.substr(1);
    }
    int id = std::stoi(idStr);
    game.updateEgg(id, true);
}

void PacketParser::handleEbo(const std::vector<std::string>& args) {
    if (args.size() < 2) {
        std::cerr << "[ERROR] ebo: Invalid number of arguments" << std::endl;
        return;
    }
    
    std::string idStr = args[1];
    if (idStr.front() == '#') {
        idStr = idStr.substr(1);
    }
    int id = std::stoi(idStr);
    game.updateEgg(id, false);
}

void PacketParser::handleEdi(const std::vector<std::string>& args) {
    if (args.size() < 2) {
        std::cerr << "[ERROR] edi: Invalid number of arguments" << std::endl;
        return;
    }
    
    std::string idStr = args[1];
    if (idStr.front() == '#') {
        idStr = idStr.substr(1);
    }
    int id = std::stoi(idStr);
    game.removeEgg(id);
}

void PacketParser::handlePic(const std::vector<std::string>& args) {
    if (args.size() < 4) {
        std::cerr << "[ERROR] pic: Invalid number of arguments" << std::endl;
        return;
    }
    
    int x = std::stoi(args[1]);
    int y = std::stoi(args[2]);
    int level = std::stoi(args[3]);
    
    std::vector<int> ids;
    for (size_t i = 4; i < args.size(); i++) {
        std::string idStr = args[i];
        if (idStr.front() == '#') {
            idStr = idStr.substr(1);
        }
        ids.push_back(std::stoi(idStr));
    }
    
    game.newElevation(x, y, level, ids);
}

void PacketParser::handlePie(const std::vector<std::string>& args) {
    if (args.size() < 4) {
        std::cerr << "[ERROR] pie: Invalid number of arguments" << std::endl;
        return;
    }
    
    int x = std::stoi(args[1]);
    int y = std::stoi(args[2]);
    int success = std::stoi(args[3]);
    
    game.updateElevation(x, y, success);
}

void PacketParser::handleSeg(const std::vector<std::string>& args) {
    if (args.size() < 2) {
        std::cerr << "[ERROR] seg: Invalid number of arguments" << std::endl;
        return;
    }
    
    game.setServerStatus(3);
    std::cout << "[DEBUG] Game ended with SEG packet" << std::endl;
    game.setWinningTeam(args[1]);
    std::cout << "[DEBUG] Winning team: " << args[1] << std::endl;
}

void PacketParser::handlePgt(const std::vector<std::string>& args) {
    if (args.size() < 3) {
        std::cerr << "[ERROR] pgt: Invalid number of arguments" << std::endl;
        return;
    }

    std::string idStr = args[1];
    if (idStr.front() == '#') {
        idStr = idStr.substr(1);
    }
    int playerId = std::stoi(idStr);
    int idComponent = std::stoi(args[2]);

    game.removeComponentId(idComponent, playerId);
}

void PacketParser::handlePdr(const std::vector<std::string>& args) {
    if (args.size() < 3) {
        std::cerr << "[ERROR] pdr: Invalid number of arguments" << std::endl;
        return;
    }
    std::string idStr = args[1];
    if (idStr.front() == '#') {
        idStr = idStr.substr(1);
    }
    int playerId = std::stoi(idStr);
    int resourceId = std::stoi(args[2]);

    game.addResourceToTile(playerId, resourceId);
}

void PacketParser::handleRemres(const std::vector<std::string>& args) {
    if (args.size() < 9) {
        std::cerr << "[ERROR] remres: Invalid number of arguments" << std::endl;
        return;
    }
    
    int x = std::stoi(args[1]);
    int y = std::stoi(args[2]);
    int linemate = std::stoi(args[3]);
    int deraumere = std::stoi(args[4]);
    int sibur = std::stoi(args[5]);
    int mendiane = std::stoi(args[6]);
    int phiras = std::stoi(args[7]);
    int thystame = std::stoi(args[8]);

    game.removeResourcesFromTile(x, y, linemate, deraumere, sibur, mendiane, phiras, thystame);
}
