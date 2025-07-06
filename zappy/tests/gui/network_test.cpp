/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** network_test
*/

#include <criterion/criterion.h>
#include <criterion/redirect.h>
#include "network/Packet.hpp"
#include "network/NetworkClient.hpp"
#include "network/PacketParser.hpp"
#include "core/Game.hpp"
#include <sstream>
#include <vector>

// Mock NetworkClient pour les tests
class MockNetworkClient {
public:
    std::vector<Packet> sent_packets;
    
    void sendPacket(const Packet& packet) {
        sent_packets.push_back(packet);
    }
    
    // Stub les autres méthodes
    void connect() {}
    void disconnect() {}
    bool isConnected() { return true; }
    std::string receivePacket() { return ""; }
    void setHost(const std::string& host) { (void)host; }
    void setPort(const std::string& port) { (void)port; }
    std::string getOption(const std::string& option, char* argv[]) { (void)option; (void)argv; return ""; }
};

// Mock Game qui hérite de Game pour être compatible avec PacketParser
class TestGame : public Game {
public:
    TestGame() : Game(0) {
        // Pas d'initialisation Raylib pour les tests
    }
    
    ~TestGame() {
        // Ne pas fermer à nouveau dans le destructeur
    }
    
    // Variables pour tester les appels
    int initialized_x = 0;
    int initialized_y = 0;
    std::vector<std::string> created_teams;
    int last_filled_x = -1, last_filled_y = -1;
    bool team_created = false;
    bool grid_initialized = false;
    bool tile_filled = false;
    
    void initializeGrid(int x, int y) {
        initialized_x = x;
        initialized_y = y;
        grid_initialized = true;
        // Appeler la méthode parent aussi
        Game::initializeGrid(x, y);
    }
    
    void createTeam(const std::string& name) {
        created_teams.push_back(name);
        team_created = true;
        // Appeler la méthode parent aussi
        Game::createTeam(name);
    }
    
    void fillTileMap(int x, int y, int q0, int q1, int q2, int q3, int q4, int q5, int q6) {
        last_filled_x = x;
        last_filled_y = y;
        tile_filled = true;
        // Appeler la méthode parent aussi
        Game::fillTileMap(x, y, q0, q1, q2, q3, q4, q5, q6);
    }
};

// Mock NetworkClient qui hérite de NetworkClient
class TestNetworkClient : public NetworkClient {
public:
    std::vector<Packet> sent_packets;
    
    void sendPacket(const Packet& packet) {
        sent_packets.push_back(packet);
        // Ne pas appeler la méthode parent pour éviter la vraie connexion
    }
};

// ========== TESTS PACKET ==========

// ========== TESTS PACKET PARSER ==========

Test(packet_parser, basic_construction) {
    TestGame game;
    TestNetworkClient client;
    
    // Tester que le constructeur ne plante pas
    try {
        PacketParser parser(game, client);
        cr_assert(true, "Constructor should work");
    } catch (...) {
        cr_assert(false, "Constructor failed");
    }
}

Test(packet_parser, handle_empty_packets) {
    TestGame game;
    TestNetworkClient client;
    PacketParser parser(game, client);
    
    // Ne devrait pas planter avec des paquets vides
    try {
        parser.parseAndExecute("");
        parser.parseAndExecute("   \n\r\t   ");
        cr_assert(true, "Empty packets should not crash");
    } catch (...) {
        cr_assert(false, "Empty packets caused crash");
    }
}

Test(packet_parser, handle_unknown_commands) {
    TestGame game;
    TestNetworkClient client;
    PacketParser parser(game, client);
    
    cr_redirect_stderr();
    
    try {
        parser.parseAndExecute("unknown_command arg1 arg2");
        cr_assert(true, "Unknown commands should not crash");
    } catch (...) {
        cr_assert(false, "Unknown commands caused crash");
    }
}

Test(packet_parser, handle_pnw_command) {
    TestGame game;
    TestNetworkClient client;
    PacketParser parser(game, client);
    
    // D'abord créer une équipe
    parser.parseAndExecute("tna testteam");
    
    // Tester la commande pnw (player new)
    try {
        parser.parseAndExecute("pnw #42 10 15 2 3 testteam");
        cr_assert(true, "pnw command executed successfully");
    } catch (...) {
        cr_assert(false, "pnw command should not crash");
    }
}

Test(packet_parser, handle_invalid_arguments) {
    TestGame game;
    TestNetworkClient client;
    PacketParser parser(game, client);
    
    cr_redirect_stderr();
    
    // Tester des commandes avec des arguments invalides
    try {
        parser.parseAndExecute("msz 10"); // Manque un argument
        parser.parseAndExecute("bct 5"); // Arguments insuffisants
        parser.parseAndExecute("tna"); // Pas d'argument
        cr_assert(true, "Invalid arguments should not crash");
    } catch (...) {
        cr_assert(false, "Invalid arguments should be handled gracefully");
    }
}

Test(packet_parser, handle_edge_case_commands) {
    TestGame game;
    TestNetworkClient client;
    PacketParser parser(game, client);
    
    // Tester des cas limites
    try {
        parser.parseAndExecute("seg winner"); // Fin de jeu
        parser.parseAndExecute("pic 0 0 1"); // Incantation
        parser.parseAndExecute("pie 0 0 1"); // Fin d'incantation
        cr_assert(true, "Edge case commands executed successfully");
    } catch (...) {
        cr_assert(false, "Edge case commands should not crash");
    }
}

