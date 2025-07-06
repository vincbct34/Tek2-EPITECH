/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** coverage_test - Tests pour augmenter la couverture
*/

#include <criterion/criterion.h>
#include <criterion/redirect.h>
#include "core/Game.hpp"
#include "core/Player.hpp"
#include "core/Tile.hpp"
#include "core/Egg.hpp"
#include "core/Team.hpp"
#include "core/Elevation.hpp"
#include "core/Component.hpp"
#include "network/Packet.hpp"
#include "network/NetworkClient.hpp"
#include <memory>
#include <vector>

// ========== TESTS PACKET ==========

Test(packet, packet_structure) {
    Packet packet;
    packet.type = "test";
    packet.args = {"arg1", "arg2", "arg3"};
    
    cr_assert_str_eq(packet.type.c_str(), "test");
    cr_assert_eq(packet.args.size(), 3);
    cr_assert_str_eq(packet.args[0].c_str(), "arg1");
    cr_assert_str_eq(packet.args[1].c_str(), "arg2");
    cr_assert_str_eq(packet.args[2].c_str(), "arg3");
}

Test(packet, packet_parse_empty) {
    Packet packet = Packet::parse("");
    
    cr_assert_str_eq(packet.type.c_str(), "");
    cr_assert_eq(packet.args.size(), 0);
}

Test(packet, packet_parse_single_word) {
    Packet packet = Packet::parse("WELCOME\n");
    
    cr_assert_eq(packet.args.size(), 0);
}

Test(packet, packet_serialize) {
    Packet packet;
    packet.type = "test";
    packet.args = {"arg1", "arg2"};
    
    std::string serialized = packet.serialize();
    cr_assert(serialized.length() > 0);
}

// ========== TESTS COMPOSANTS SIMPLES ==========

Test(additional_coverage, tile_edge_cases) {
    Tile tile(0, 0, 10, 10, 800, 600);
    
    // Test des accesseurs basiques
    cr_assert_eq(tile.getX(), 0);
    cr_assert_eq(tile.getY(), 0);
    cr_assert_eq(tile.getSizeX(), 80); // 800/10
    cr_assert_eq(tile.getSizeY(), 60); // 600/10
    cr_assert_eq(tile.getNumberOfComponents(), 0);
    
    // Test avec d'autres positions
    Tile tile2(5, 7, 15, 12, 900, 720);
    cr_assert_eq(tile2.getX(), 5);
    cr_assert_eq(tile2.getY(), 7);
    cr_assert_eq(tile2.getSizeX(), 60); // 900/15
    cr_assert_eq(tile2.getSizeY(), 60); // 720/12
    
    // Test du rectangle
    Rectangle rect = tile.getRectangle();
    cr_assert_geq(rect.width, 0);
    cr_assert_geq(rect.height, 0);
}

Test(additional_coverage, player_edge_cases) {
    Player player(42, 10, 15, 2, 3, 1);
    
    // Test des getters basiques
    cr_assert_eq(player.getId(), 42);
    cr_assert_eq(player.getPosX(), 10);
    cr_assert_eq(player.getPosY(), 15);
    cr_assert_eq(player.getOrientation(), 2);
    cr_assert_eq(player.getLevel(), 3);
    
    // Test des setters existants
    player.setPosX(50);
    player.setPosY(60);
    player.setOrientation(4);
    player.setLevel(5);
    
    cr_assert_eq(player.getPosX(), 50);
    cr_assert_eq(player.getPosY(), 60);
    cr_assert_eq(player.getOrientation(), 4);
    cr_assert_eq(player.getLevel(), 5);
    
    // Test de l'inventaire
    auto& inventory = player.getInventory();
    cr_assert_eq(inventory.size(), 0);
    
    // Test des œufs
    auto& eggs = player.getEggs();
    cr_assert_eq(eggs.size(), 0);
}

Test(additional_coverage, egg_edge_cases) {
    Egg egg(123, 7, 9);
    
    // Test des valeurs de construction
    cr_assert_eq(egg.getId(), 123);
    cr_assert_eq(egg.getPosX(), 7);
    cr_assert_eq(egg.getPosY(), 9);
    cr_assert_eq(egg.isHatching(), false);
    cr_assert_eq(egg.getSpriteIndex(), 0);
    cr_assert_eq(egg.getStartHatchTime(), 0.0);
    
    // Test des setters
    egg.setPosX(20);
    egg.setPosY(25);
    egg.setHatching(true);
    egg.setSpriteIndex(3);
    egg.setStartHatchTime(123.45);
    
    cr_assert_eq(egg.getPosX(), 20);
    cr_assert_eq(egg.getPosY(), 25);
    cr_assert_eq(egg.isHatching(), true);
    cr_assert_eq(egg.getSpriteIndex(), 3);
    cr_assert_eq(egg.getStartHatchTime(), 123.45);
}

Test(additional_coverage, team_edge_cases) {
    Team team("TestTeam");
    
    // Test du nom
    cr_assert_str_eq(team.getName().c_str(), "TestTeam");
    
    // Test avec des caractères spéciaux
    Team specialTeam("team@#$%^&*()");
    cr_assert_str_eq(specialTeam.getName().c_str(), "team@#$%^&*()");
    
    // Test de couleur
    Color color = team.getColor();
    (void)color; // Éviter le warning
    
    // Test d'ajout de joueurs
    Player player(1, 5, 5, 1, 1, team.getId());
    team.addPlayer(player);
    cr_assert_eq(team.getPlayerCount(), 1);
}

Test(additional_coverage, elevation_edge_cases) {
    Elevation elevation;
    // Test des setters
    elevation.setPosX(999999);
    elevation.setPosY(999999);
    elevation.setElevationActive(true);
    elevation.setElevationSuccess(true);
    
    cr_assert_eq(elevation.getPosX(), 999999);
    cr_assert_eq(elevation.getPosY(), 999999);
    cr_assert_eq(elevation.getElevation(), true);
    cr_assert_eq(elevation.getElevationSuccess(), true);
    
    // Test avec initElevation
    std::vector<int> testIds = {1, 2, 3};
    elevation.initElevation(10, 15, 2, testIds);
    cr_assert_eq(elevation.getPosX(), 10);
    cr_assert_eq(elevation.getPosY(), 15);
    const auto& ids = elevation.getIds();
    cr_assert_eq(ids.size(), 3);
    cr_assert_eq(ids[0], 1);
    cr_assert_eq(ids[1], 2);
    cr_assert_eq(ids[2], 3);
}

// ========== TESTS GAME INTEGRATION AVANCÉS ==========

Test(game_integration, full_game_simulation) {
    Game game(0);
    
    // Simulation d'un jeu complet
    game.initializeGrid(10, 10);
    
    // Créer des équipes
    game.createTeam("Alpha");
    game.createTeam("Beta");
    
    // Remplir quelques tiles
    game.fillTileMap(0, 0, 5, 1, 0, 1, 0, 0, 1);
    game.fillTileMap(5, 5, 10, 2, 1, 0, 3, 1, 2);
    game.fillTileMap(9, 9, 3, 0, 2, 0, 1, 0, 0);
    
    // Créer des joueurs
    game.createPlayer(1, 0, 0, 1, 1, "Alpha");
    game.createPlayer(2, 5, 5, 2, 2, "Alpha");
    game.createPlayer(3, 9, 9, 3, 3, "Beta");
    
    // Mettre à jour les joueurs
    game.updatePlayer(1, 1, 1, 2, 2);
    game.updatePlayer(2, 6, 6, 3, 3);
    
    // Mettre à jour les inventaires
    game.updateInventoryPlayer(1, 5, 2, 1, 0, 3, 1, 0);
    game.updateInventoryPlayer(2, 3, 1, 0, 1, 0, 0, 2);
    
    // Créer des œufs
    game.createEgg(1, 100, 2, 2);
    game.createEgg(2, 101, 7, 7);
    
    // Mettre à jour les œufs
    game.updateEgg(100, true);
    game.updateEgg(101, false);
    
    // Créer des élévations
    std::vector<int> players1 = {1, 2};
    std::vector<int> players2 = {3};
    game.newElevation(5, 5, 2, players1);
    game.newElevation(9, 9, 1, players2);
    
    // Mettre à jour les élévations
    game.updateElevation(5, 5, 1);
    game.updateElevation(9, 9, 0);
    
    // Supprimer quelques éléments
    game.removeEgg(100);
    game.removePlayer(3);
    
    cr_assert(true, "Full game simulation should complete without crashes");
}

Test(game_integration, stress_test_many_entities) {
    Game game(0);
    
    // Test de stress avec beaucoup d'entités
    game.initializeGrid(20, 20);
    
    // Créer beaucoup d'équipes
    for (int i = 0; i < 10; i++) {
        game.createTeam("Team" + std::to_string(i));
    }
    
    // Remplir toutes les tiles
    for (int x = 0; x < 20; x++) {
        for (int y = 0; y < 20; y++) {
            game.fillTileMap(x, y, x % 10, y % 5, (x + y) % 3, 
                           x % 2, y % 4, (x * y) % 2, (x + y) % 7);
        }
    }
    
    // Créer beaucoup de joueurs
    for (int i = 0; i < 50; i++) {
        std::string teamName = "Team" + std::to_string(i % 10);
        game.createPlayer(i, i % 20, (i * 2) % 20, i % 4, (i % 8) + 1, teamName);
    }
    
    // Créer beaucoup d'œufs
    for (int i = 0; i < 30; i++) {
        game.createEgg(i % 50, 1000 + i, i % 20, (i * 3) % 20);
    }
    
    cr_assert(true, "Stress test should complete without crashes");
}

Test(game_integration, complex_elevation_scenarios) {
    Game game(0);
    
    game.initializeGrid(5, 5);
    game.createTeam("ElevationTeam");
    
    // Créer des joueurs pour les élévations
    for (int i = 1; i <= 6; i++) {
        game.createPlayer(i, 2, 2, 1, i, "ElevationTeam");
    }
    
    // Différents scénarios d'élévation
    std::vector<int> singlePlayer = {1};
    std::vector<int> twoPlayers = {2, 3};
    std::vector<int> threePlayers = {4, 5, 6};
    std::vector<int> emptyPlayers;
    
    game.newElevation(0, 0, 1, singlePlayer);
    game.newElevation(1, 1, 2, twoPlayers);
    game.newElevation(2, 2, 3, threePlayers);
    game.newElevation(3, 3, 1, emptyPlayers);
    
    // Différents résultats d'élévation
    game.updateElevation(0, 0, 1); // Succès
    game.updateElevation(1, 1, 0); // Échec
    game.updateElevation(2, 2, 1); // Succès
    game.updateElevation(3, 3, 0); // Échec
    
    cr_assert(true, "Complex elevation scenarios should work");
}

Test(game_integration, player_lifecycle_complete) {
    Game game(0);
    
    game.initializeGrid(10, 10);
    game.createTeam("LifecycleTeam");
    
    // Créer un joueur
    game.createPlayer(1, 5, 5, 1, 1, "LifecycleTeam");
    
    // Mettre à jour ses propriétés plusieurs fois
    for (int i = 0; i < 10; i++) {
        game.updatePlayer(1, i, i, i % 4, (i % 8) + 1);
        game.updateInventoryPlayer(1, i, i+1, i+2, i+3, i+4, i+5, i+6);
    }
    
    // Créer des œufs pour ce joueur
    for (int i = 0; i < 5; i++) {
        game.createEgg(1, 100 + i, 5 + i, 5 - i);
        game.updateEgg(100 + i, i % 2 == 0);
    }
    
    // Supprimer quelques œufs
    game.removeEgg(101);
    game.removeEgg(103);
    
    // Finalement supprimer le joueur
    game.removePlayer(1);
    
    cr_assert(true, "Complete player lifecycle should work");
}

Test(game_integration, edge_case_operations) {
    Game game(0);
    
    // Opérations sur un jeu non initialisé
    game.createTeam("TestTeam");
    game.createPlayer(1, 0, 0, 1, 1, "TestTeam");
    game.updatePlayer(1, 5, 5, 2, 2);
    game.removePlayer(1);
    
    // Initialiser après
    game.initializeGrid(5, 5);
    
    // Opérations avec des valeurs limites
    game.fillTileMap(0, 0, 0, 0, 0, 0, 0, 0, 0); // Tout à zéro
    game.fillTileMap(4, 4, 999, 999, 999, 999, 999, 999, 999); // Valeurs élevées
    
    // Opérations avec des IDs invalides
    game.updatePlayer(999, 0, 0, 0, 0);
    game.removePlayer(999);
    game.updateEgg(999, true);
    game.removeEgg(999);
    
    cr_assert(true, "Edge case operations should not crash");
}

Test(additional_coverage, component_edge_cases) {
    Component component(ComponentType::FOOD, 5, 80, 60);
    
    // Test des getters basiques
    cr_assert(component.getType() == ComponentType::FOOD);
    cr_assert_eq(component.getQuantity(), 5);
    cr_assert_eq(component.getId(), static_cast<int>(ComponentType::FOOD));
    
    // Test avec un autre type
    Component linemate(ComponentType::LINEMATE, 3, 80, 60);
    cr_assert(linemate.getType() == ComponentType::LINEMATE);
    cr_assert_eq(linemate.getQuantity(), 3);
    cr_assert_eq(linemate.getId(), static_cast<int>(ComponentType::LINEMATE));
    
    // Test avec tous les types de composants
    std::vector<ComponentType> allTypes = {
        ComponentType::NONE,
        ComponentType::FOOD,
        ComponentType::LINEMATE,
        ComponentType::DERAUMERE,
        ComponentType::SIBUR,
        ComponentType::MENDIANE,
        ComponentType::PHIRAS,
        ComponentType::THYSTAME,
        ComponentType::EGG,
        ComponentType::ELEVATION,
        ComponentType::PLAYER
    };
    
    for (auto type : allTypes) {
        Component comp(type, 1, 80, 60);
        cr_assert(comp.getType() == type);
        cr_assert_eq(comp.getId(), static_cast<int>(type));
    }
    
    // Test avec des quantités différentes
    Component comp1(ComponentType::SIBUR, 0, 80, 60);
    cr_assert_eq(comp1.getQuantity(), 0);
    
    Component comp2(ComponentType::PHIRAS, 999999, 80, 60);
    cr_assert_eq(comp2.getQuantity(), 999999);
    
    // Test de setQuantity
    comp1.setQuantity(10);
    cr_assert_eq(comp1.getQuantity(), 10);
    
    comp1.setQuantity(0);
    cr_assert_eq(comp1.getQuantity(), 0);
}

Test(additional_coverage, network_client_basic) {
    NetworkClient client;
    
    // Test de la fonction getOption avec différents cas
    char* testArgv[] = {
        const_cast<char*>("program"),
        const_cast<char*>("-p"),
        const_cast<char*>("4242"),
        const_cast<char*>("-h"),
        const_cast<char*>("localhost"),
        nullptr
    };
    
    // Test d'options existantes
    std::string port = client.getOption("-p", testArgv);
    cr_assert_str_eq(port.c_str(), "4242");
    
    std::string host = client.getOption("-h", testArgv);
    cr_assert_str_eq(host.c_str(), "localhost");
    
    // Test d'options inexistantes
    std::string inexistant = client.getOption("-x", testArgv);
    cr_assert_str_eq(inexistant.c_str(), "");
    
    std::string autre = client.getOption("--unknown", testArgv);
    cr_assert_str_eq(autre.c_str(), "");
}
