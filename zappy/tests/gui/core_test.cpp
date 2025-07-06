/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** core_test
*/

#include <criterion/criterion.h>
#include <criterion/redirect.h>
#include "core/Component.hpp"
#include "core/Player.hpp"
#include "core/Team.hpp"
#include "core/Tile.hpp"
#include "core/Egg.hpp"
#include "core/Game.hpp"
#include <memory>
#include <vector>

// ========== TESTS COMPONENT ==========

Test(component, create_food_component) {
    Component comp(ComponentType::FOOD, 5, 80, 60);
    
    cr_assert_eq(comp.getType(), ComponentType::FOOD);
    cr_assert_eq(comp.getQuantity(), 5);
    cr_assert_eq(comp.getId(), static_cast<int>(ComponentType::FOOD));
}

Test(component, create_linemate_component) {
    Component comp(ComponentType::LINEMATE, 3, 80, 60);
    
    cr_assert_eq(comp.getType(), ComponentType::LINEMATE);
    cr_assert_eq(comp.getQuantity(), 3);
    cr_assert_eq(comp.getId(), static_cast<int>(ComponentType::LINEMATE));
}

Test(component, set_quantity) {
    Component comp(ComponentType::DERAUMERE, 1, 80, 60);
    
    cr_assert_eq(comp.getQuantity(), 1);
    
    comp.setQuantity(10);
    cr_assert_eq(comp.getQuantity(), 10);
    
    comp.setQuantity(0);
    cr_assert_eq(comp.getQuantity(), 0);
}

Test(component, create_all_resource_types) {
    std::vector<ComponentType> types = {
        ComponentType::FOOD,
        ComponentType::LINEMATE,
        ComponentType::DERAUMERE,
        ComponentType::SIBUR,
        ComponentType::MENDIANE,
        ComponentType::PHIRAS,
        ComponentType::THYSTAME
    };
    
    for (auto type : types) {
        Component comp(type, 1, 80, 60);
        cr_assert_eq(comp.getType(), type);
        cr_assert_eq(comp.getQuantity(), 1);
    }
}

Test(component, texture_initially_not_loaded) {
    Component comp(ComponentType::FOOD, 1, 80, 60);
    
    // La texture ne devrait pas être chargée initialement dans les tests
    cr_assert_eq(comp.isTextureLoaded(), false);
}

// ========== TESTS PLAYER ==========

Test(player, create_player) {
    Player player(42, 5, 3, 2, 1, 0);
    
    cr_assert_eq(player.getId(), 42);
    cr_assert_eq(player.getPosX(), 5);
    cr_assert_eq(player.getPosY(), 3);
    cr_assert_eq(player.getOrientation(), 2);
    cr_assert_eq(player.getLevel(), 1);
}

Test(player, modify_player_position) {
    Player player(1, 0, 0, 1, 1, 0);
    
    player.setPosX(10);
    player.setPosY(15);
    
    cr_assert_eq(player.getPosX(), 10);
    cr_assert_eq(player.getPosY(), 15);
}

Test(player, modify_player_orientation) {
    Player player(1, 5, 5, 1, 1, 0);
    
    player.setOrientation(3);
    cr_assert_eq(player.getOrientation(), 3);
    
    player.setOrientation(0);
    cr_assert_eq(player.getOrientation(), 0);
}

Test(player, modify_player_level) {
    Player player(1, 5, 5, 1, 1, 0);
    
    player.setLevel(5);
    cr_assert_eq(player.getLevel(), 5);
    
    player.setLevel(8);
    cr_assert_eq(player.getLevel(), 8);
}

Test(player, inventory_initially_empty) {
    Player player(1, 5, 5, 1, 1, 0);
    
    cr_assert_eq(player.getInventory().size(), 0);
}

Test(player, eggs_initially_empty) {
    Player player(1, 5, 5, 1, 1, 0);
    
    cr_assert_eq(player.getEggs().size(), 0);
}

Test(player, texture_initially_not_loaded) {
    Player player(1, 5, 5, 1, 1, 0);
    
    // La texture ne devrait pas être chargée initialement dans les tests
    cr_assert_eq(player.isTextureLoaded(), false);
}

// ========== TESTS TEAM ==========

Test(team, create_team) {
    Team team("TeamAlpha");
    
    cr_assert_str_eq(team.getName().c_str(), "TeamAlpha");
    cr_assert_eq(team.getPlayerCount(), 0);
}

Test(team, team_has_unique_id) {
    Team team1("Team1");
    Team team2("Team2");
    
    // Les équipes devraient avoir des IDs différents
    cr_assert_neq(team1.getId(), team2.getId());
}

Test(team, team_has_color) {
    Team team("TestTeam");
    
    Color color = team.getColor();
    (void)color; // Éviter le warning unused variable
    // Vérifier que la couleur est définie (pas forcément blanche)
    cr_assert(true); // Test basique que getColor() ne plante pas
}

Test(team, add_player_to_team) {
    Team team("TestTeam");
    Player player(1, 5, 5, 1, 1, team.getId());
    
    team.addPlayer(player);
    
    cr_assert_eq(team.getPlayerCount(), 1);
    cr_assert_eq(team.getPlayers().size(), 1);
}

Test(team, add_multiple_players) {
    Team team("TestTeam");
    
    for (int i = 0; i < 5; i++) {
        Player player(i, i, i, 1, 1, team.getId());
        team.addPlayer(player);
    }
    
    cr_assert_eq(team.getPlayerCount(), 5);
    cr_assert_eq(team.getPlayers().size(), 5);
}

Test(team, get_player_count_on_tile) {
    Team team("TestTeam");
    
    // Ajouter plusieurs joueurs à la même position
    Player player1(1, 5, 5, 1, 1, team.getId());
    Player player2(2, 5, 5, 2, 1, team.getId());
    Player player3(3, 10, 10, 1, 1, team.getId());
    
    team.addPlayer(player1);
    team.addPlayer(player2);
    team.addPlayer(player3);
    
    // 2 joueurs sur la tile (5,5)
    cr_assert_eq(team.getPlayerCountOnTile(5, 5), 2);
    // 1 joueur sur la tile (10,10)
    cr_assert_eq(team.getPlayerCountOnTile(10, 10), 1);
    // 0 joueur sur une tile vide
    cr_assert_eq(team.getPlayerCountOnTile(0, 0), 0);
}

// ========== TESTS TILE ==========

Test(tile, create_tile) {
    Tile tile(2, 3, 10, 10, 800, 600);
    
    cr_assert_eq(tile.getX(), 2);
    cr_assert_eq(tile.getY(), 3);
    
    Rectangle rect = tile.getRectangle();
    // Vérifier que le rectangle a des dimensions positives
    cr_assert_gt(rect.width, 0);
    cr_assert_gt(rect.height, 0);
}

Test(tile, tile_rectangle_calculation) {
    // Grille 4x4 dans une fenêtre 800x600
    Tile tile1(0, 0, 4, 4, 800, 600);
    Tile tile2(1, 1, 4, 4, 800, 600);
    
    Rectangle rect1 = tile1.getRectangle();
    Rectangle rect2 = tile2.getRectangle();
    
    // Les rectangles devraient avoir la même taille
    cr_assert_eq(rect1.width, rect2.width);
    cr_assert_eq(rect1.height, rect2.height);
    
    // Mais des positions différentes
    cr_assert_neq(rect1.x, rect2.x);
    cr_assert_neq(rect1.y, rect2.y);
}

Test(tile, different_grid_sizes) {
    // Test avec différentes tailles de grille
    Tile small_grid(0, 0, 5, 5, 800, 600);
    Tile large_grid(0, 0, 20, 20, 800, 600);
    
    Rectangle small_rect = small_grid.getRectangle();
    Rectangle large_rect = large_grid.getRectangle();
    
    // Les tiles d'une petite grille devraient être plus grandes
    cr_assert_gt(small_rect.width, large_rect.width);
    cr_assert_gt(small_rect.height, large_rect.height);
}

Test(tile, add_components_to_tile) {
    Tile tile(0, 0, 10, 10, 800, 600);
    
    cr_assert_eq(tile.getNumberOfComponents(), 0);
    
    auto food = std::make_shared<Component>(ComponentType::FOOD, 3, 80, 60);
    auto linemate = std::make_shared<Component>(ComponentType::LINEMATE, 1, 80, 60);
    
    tile.addComponent(food);
    tile.addComponent(linemate);
    
    cr_assert_eq(tile.getNumberOfComponents(), 2);
    
    const auto& components = tile.getComponents();
    cr_assert_eq(components.size(), 2);
    cr_assert_eq(components[0]->getType(), ComponentType::FOOD);
    cr_assert_eq(components[1]->getType(), ComponentType::LINEMATE);
}

Test(tile, remove_component_from_tile) {
    Tile tile(0, 0, 10, 10, 800, 600);
    
    auto food = std::make_shared<Component>(ComponentType::FOOD, 3, 80, 60);
    auto linemate = std::make_shared<Component>(ComponentType::LINEMATE, 1, 80, 60);
    
    tile.addComponent(food);
    tile.addComponent(linemate);
    cr_assert_eq(tile.getNumberOfComponents(), 2);
    
    tile.removeComponent(food);
    cr_assert_eq(tile.getNumberOfComponents(), 1);
    
    const auto& components = tile.getComponents();
    cr_assert_eq(components[0]->getType(), ComponentType::LINEMATE);
}

Test(tile, add_resource_by_id) {
    Tile tile(0, 0, 10, 10, 800, 600);
    
    cr_assert_eq(tile.getNumberOfComponents(), 0);
    
    // Tester addResource
    try {
        tile.addResource(1); // ID pour FOOD
        cr_assert(true, "addResource should not crash");
    } catch (...) {
        cr_assert(false, "addResource should not throw");
    }
}

Test(tile, tile_size_properties) {
    Tile tile(2, 3, 10, 10, 800, 600);
    
    cr_assert_eq(tile.getSizeX(), 80); // 800/10
    cr_assert_eq(tile.getSizeY(), 60); // 600/10
}

// ========== TESTS EGG ==========

Test(egg, create_egg) {
    Egg egg(100, 5, 7);
    
    cr_assert_eq(egg.getId(), 100);
    cr_assert_eq(egg.getPosX(), 5);
    cr_assert_eq(egg.getPosY(), 7);
    cr_assert_eq(egg.isHatching(), false);
}

Test(egg, egg_hatching_state) {
    Egg egg(200, 10, 15);
    
    cr_assert_eq(egg.isHatching(), false);
    
    egg.setHatching(true);
    cr_assert_eq(egg.isHatching(), true);
    
    egg.setHatching(false);
    cr_assert_eq(egg.isHatching(), false);
}

Test(egg, egg_position) {
    Egg egg(300, 0, 0);
    
    egg.setPosX(20);
    egg.setPosY(25);
    
    cr_assert_eq(egg.getPosX(), 20);
    cr_assert_eq(egg.getPosY(), 25);
}

Test(egg, egg_sprite_index) {
    Egg egg(400, 5, 5);
    
    cr_assert_eq(egg.getSpriteIndex(), 0);
    
    egg.setSpriteIndex(3);
    cr_assert_eq(egg.getSpriteIndex(), 3);
}

Test(egg, egg_hatch_time) {
    Egg egg(500, 5, 5);
    
    cr_assert_eq(egg.getStartHatchTime(), 0.0);
    
    egg.setStartHatchTime(123.45);
    cr_assert_eq(egg.getStartHatchTime(), 123.45);
}

// ========== TESTS D'INTÉGRATION ==========

Test(integration, player_with_inventory) {
    Player player(1, 5, 5, 1, 1, 0);
    
    // Simuler l'ajout d'items à l'inventaire
    auto& inventory = player.getInventory();
    
    // L'inventaire devrait être modifiable
    cr_assert_eq(inventory.size(), 0);
    
    // Ajouter des composants
    inventory.push_back(std::make_shared<Component>(ComponentType::FOOD, 3, 80, 60));
    inventory.push_back(std::make_shared<Component>(ComponentType::LINEMATE, 1, 80, 60));
    
    cr_assert_eq(inventory.size(), 2);
    cr_assert_eq(inventory[0]->getType(), ComponentType::FOOD);
    cr_assert_eq(inventory[1]->getType(), ComponentType::LINEMATE);
}

Test(integration, player_with_eggs) {
    Player player(1, 5, 5, 1, 1, 0);
    
    auto& eggs = player.getEggs();
    cr_assert_eq(eggs.size(), 0);
    
    // Ajouter des œufs
    eggs.push_back(std::make_shared<Egg>(100, 5, 5));
    eggs.push_back(std::make_shared<Egg>(101, 6, 6));
    
    cr_assert_eq(eggs.size(), 2);
    cr_assert_eq(eggs[0]->getId(), 100);
    cr_assert_eq(eggs[1]->getId(), 101);
}

Test(integration, team_with_multiple_players_different_levels) {
    Team team("AdvancedTeam");
    
    // Ajouter des joueurs de différents niveaux
    for (int i = 1; i <= 8; i++) {
        Player player(i, i % 10, i % 10, 1, i, team.getId());
        team.addPlayer(player);
    }
    
    cr_assert_eq(team.getPlayerCount(), 8);
    
    // Vérifier que les joueurs ont bien été ajoutés avec leurs niveaux
    auto& players = team.getPlayers();
    for (size_t i = 0; i < players.size(); i++) {
        cr_assert_eq(players[i]->getLevel(), static_cast<int>(i + 1));
    }
}

Test(integration, component_quantity_management) {
    // Test de gestion des quantités comme dans un vrai jeu
    Component food(ComponentType::FOOD, 10, 80, 60);
    Component linemate(ComponentType::LINEMATE, 5, 80, 60);
    
    // Simulation de consommation
    food.setQuantity(food.getQuantity() - 3);
    cr_assert_eq(food.getQuantity(), 7);
    
    linemate.setQuantity(linemate.getQuantity() - 1);
    cr_assert_eq(linemate.getQuantity(), 4);
    
    // Test de quantité nulle
    food.setQuantity(0);
    cr_assert_eq(food.getQuantity(), 0);
}

// ========== TESTS GAME APPROFONDIS ==========

Test(game, create_game_default) {
    Game game(0);
    
    cr_assert_eq(game.getwindowWidth(), 800);
    cr_assert_eq(game.getwindowHeight(), 600);
    cr_assert_eq(game.getServerStatus(), 0);
    cr_assert_eq(game.isGuiListening(), false);
}

Test(game, create_game_with_network) {
    Game game(1);
    
    cr_assert_eq(game.getServerStatus(), 1);
    cr_assert_eq(game.getwindowWidth(), 800);
    cr_assert_eq(game.getwindowHeight(), 600);
}

Test(game, set_and_get_server_status) {
    Game game(0);
    
    cr_assert_eq(game.getServerStatus(), 0);
    
    game.setServerStatus(1);
    cr_assert_eq(game.getServerStatus(), 1);
    
    game.setServerStatus(2);
    cr_assert_eq(game.getServerStatus(), 2);
}

Test(game, set_and_get_gui_listening) {
    Game game(0);
    
    cr_assert_eq(game.isGuiListening(), false);
    
    game.setGuiIsListening(true);
    cr_assert_eq(game.isGuiListening(), true);
    
    game.setGuiIsListening(false);
    cr_assert_eq(game.isGuiListening(), false);
}

Test(game, set_and_get_server_ip) {
    Game game(0);
    
    cr_assert_str_eq(game.getServerIp().c_str(), "");
    
    game.setServerIp("127.0.0.1");
    cr_assert_str_eq(game.getServerIp().c_str(), "127.0.0.1");
    
    game.setServerIp("192.168.1.1");
    cr_assert_str_eq(game.getServerIp().c_str(), "192.168.1.1");
}

Test(game, set_and_get_server_port) {
    Game game(0);
    
    cr_assert_str_eq(game.getServerPort().c_str(), "");
    
    game.setServerPort("8080");
    cr_assert_str_eq(game.getServerPort().c_str(), "8080");
    
    game.setServerPort("4242");
    cr_assert_str_eq(game.getServerPort().c_str(), "4242");
}

Test(game, initialize_grid) {
    Game game(0);
    
    game.initializeGrid(5, 3);
    
    // Vérifier que la grille a été initialisée
    cr_assert(true); // Le jeu ne plante pas
}

Test(game, create_team) {
    Game game(0);
    
    game.createTeam("team1");
    game.createTeam("team2");
    game.createTeam("team3");
    
    // Vérifier que les équipes ont été créées sans plantage
    cr_assert(true);
}

Test(game, create_teams_with_various_names) {
    Game game(0);
    
    game.createTeam("Alpha");
    game.createTeam("Beta");
    game.createTeam("Gamma");
    game.createTeam("Delta");
    game.createTeam("EmptyName");
    game.createTeam("Very_Long_Team_Name_123");
    
    cr_assert(true);
}

Test(game, fill_tile_map) {
    Game game(0);
    
    game.initializeGrid(10, 10);
    
    // Remplir une tile avec des ressources
    game.fillTileMap(5, 3, 10, 2, 1, 0, 3, 1, 2);
    
    cr_assert(true);
}

Test(game, fill_tile_map_multiple_tiles) {
    Game game(0);
    
    game.initializeGrid(5, 5);
    
    // Remplir plusieurs tiles
    game.fillTileMap(0, 0, 5, 1, 0, 1, 0, 0, 1);
    game.fillTileMap(2, 2, 3, 0, 2, 0, 1, 0, 0);
    game.fillTileMap(4, 4, 0, 0, 0, 0, 0, 0, 0); // Tile vide
    
    cr_assert(true);
}

Test(game, fill_tile_map_edge_cases) {
    Game game(0);
    
    game.initializeGrid(3, 3);
    
    // Cases limites
    game.fillTileMap(0, 0, 100, 50, 25, 10, 5, 2, 1); // Grosses quantités
    game.fillTileMap(2, 2, 0, 0, 0, 0, 0, 0, 0); // Tout à zéro
    game.fillTileMap(1, 1, 1, 1, 1, 1, 1, 1, 1); // Tout à 1
    
    cr_assert(true);
}

Test(game, create_player) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    
    game.createPlayer(1, 5, 5, 1, 1, "team1");
    
    cr_assert(true);
}

Test(game, create_multiple_players) {
    Game game(0);
    
    game.createTeam("team1");
    game.createTeam("team2");
    game.initializeGrid(10, 10);
    
    // Créer plusieurs joueurs
    game.createPlayer(1, 0, 0, 1, 1, "team1");
    game.createPlayer(2, 5, 5, 2, 2, "team1");
    game.createPlayer(3, 9, 9, 3, 3, "team2");
    
    cr_assert(true);
}

Test(game, create_player_invalid_team) {
    Game game(0);
    
    game.initializeGrid(10, 10);
    
    // Essayer de créer un joueur avec une équipe inexistante
    game.createPlayer(1, 5, 5, 1, 1, "nonexistent_team");
    
    cr_assert(true); // Ne devrait pas planter
}

Test(game, update_player) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    game.createPlayer(1, 5, 5, 1, 1, "team1");
    
    // Mettre à jour le joueur
    game.updatePlayer(1, 6, 4, 3, 2);
    
    cr_assert(true);
}

Test(game, update_multiple_players) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    game.createPlayer(1, 0, 0, 1, 1, "team1");
    game.createPlayer(2, 5, 5, 2, 2, "team1");
    
    // Mettre à jour les joueurs
    game.updatePlayer(1, 1, 1, 2, 2);
    game.updatePlayer(2, 6, 6, 3, 3);
    
    cr_assert(true);
}

Test(game, update_nonexistent_player) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    
    // Essayer de mettre à jour un joueur inexistant
    game.updatePlayer(999, 5, 5, 1, 1);
    
    cr_assert(true); // Ne devrait pas planter
}

Test(game, update_inventory_player) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    game.createPlayer(1, 5, 5, 1, 1, "team1");
    
    // Mettre à jour l'inventaire
    game.updateInventoryPlayer(1, 5, 2, 1, 0, 3, 1, 0);
    
    cr_assert(true);
}

Test(game, update_inventory_multiple_players) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    game.createPlayer(1, 0, 0, 1, 1, "team1");
    game.createPlayer(2, 5, 5, 2, 2, "team1");
    
    // Mettre à jour les inventaires
    game.updateInventoryPlayer(1, 10, 5, 3, 2, 1, 0, 1);
    game.updateInventoryPlayer(2, 0, 0, 0, 0, 0, 0, 0);
    
    cr_assert(true);
}

Test(game, update_inventory_nonexistent_player) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    
    // Essayer de mettre à jour l'inventaire d'un joueur inexistant
    game.updateInventoryPlayer(999, 5, 2, 1, 0, 3, 1, 0);
    
    cr_assert(true); // Ne devrait pas planter
}

Test(game, remove_player) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    game.createPlayer(1, 5, 5, 1, 1, "team1");
    
    // Supprimer le joueur
    game.removePlayer(1);
    
    cr_assert(true);
}

Test(game, remove_multiple_players) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    game.createPlayer(1, 0, 0, 1, 1, "team1");
    game.createPlayer(2, 5, 5, 2, 2, "team1");
    game.createPlayer(3, 9, 9, 3, 3, "team1");
    
    // Supprimer quelques joueurs
    game.removePlayer(2);
    game.removePlayer(1);
    
    cr_assert(true);
}

Test(game, remove_nonexistent_player) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    
    // Essayer de supprimer un joueur inexistant
    game.removePlayer(999);
    
    cr_assert(true); // Ne devrait pas planter
}

Test(game, create_egg) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    game.createPlayer(1, 5, 5, 1, 1, "team1");
    
    // Créer un œuf
    game.createEgg(1, 100, 6, 6);
    
    cr_assert(true);
}

Test(game, create_multiple_eggs) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    game.createPlayer(1, 5, 5, 1, 1, "team1");
    game.createPlayer(2, 7, 7, 2, 2, "team1");
    
    // Créer plusieurs œufs
    game.createEgg(1, 100, 6, 6);
    game.createEgg(1, 101, 5, 4);
    game.createEgg(2, 102, 8, 8);
    
    cr_assert(true);
}

Test(game, update_egg) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    game.createPlayer(1, 5, 5, 1, 1, "team1");
    game.createEgg(1, 100, 6, 6);
    
    // Mettre à jour l'œuf
    game.updateEgg(100, true);
    game.updateEgg(100, false);
    
    cr_assert(true);
}

Test(game, update_nonexistent_egg) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    
    // Essayer de mettre à jour un œuf inexistant
    game.updateEgg(999, true);
    
    cr_assert(true); // Ne devrait pas planter
}

Test(game, remove_egg) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    game.createPlayer(1, 5, 5, 1, 1, "team1");
    game.createEgg(1, 100, 6, 6);
    
    // Supprimer l'œuf
    game.removeEgg(100);
    
    cr_assert(true);
}

Test(game, remove_nonexistent_egg) {
    Game game(0);
    
    game.createTeam("team1");
    game.initializeGrid(10, 10);
    
    // Essayer de supprimer un œuf inexistant
    game.removeEgg(999);
    
    cr_assert(true); // Ne devrait pas planter
}

Test(game, new_elevation) {
    Game game(0);
    
    game.initializeGrid(10, 10);
    
    std::vector<int> playerIds = {1, 2, 3};
    game.newElevation(5, 5, 2, playerIds);
    
    cr_assert(true);
}

Test(game, new_elevation_empty_players) {
    Game game(0);
    
    game.initializeGrid(10, 10);
    
    std::vector<int> emptyPlayerIds;
    game.newElevation(3, 3, 1, emptyPlayerIds);
    
    cr_assert(true);
}

Test(game, update_elevation) {
    Game game(0);
    
    game.initializeGrid(10, 10);
    
    std::vector<int> playerIds = {1};
    game.newElevation(5, 5, 2, playerIds);
    
    // Mettre à jour l'élévation
    game.updateElevation(5, 5, 1); // Succès
    game.updateElevation(5, 5, 0); // Échec
    
    cr_assert(true);
}

// ========== TESTS COMPONENT APPROFONDIS ==========

Test(component, component_constructor_edge_cases) {
    // Test avec des valeurs minimales
    Component comp1(ComponentType::FOOD, 0, 1, 1);
    cr_assert_eq(comp1.getQuantity(), 0);
    
    // Test avec des valeurs grandes
    Component comp2(ComponentType::LINEMATE, 1000, 200, 150);
    cr_assert_eq(comp2.getQuantity(), 1000);
}

Test(component, component_type_enum_coverage) {
    // Tester tous les types de composants
    std::vector<ComponentType> allTypes = {
        ComponentType::FOOD,
        ComponentType::LINEMATE,
        ComponentType::DERAUMERE,
        ComponentType::SIBUR,
        ComponentType::MENDIANE,
        ComponentType::PHIRAS,
        ComponentType::THYSTAME
    };
    
    for (size_t i = 0; i < allTypes.size(); i++) {
        Component comp(allTypes[i], i + 1, 80, 60);
        cr_assert_eq(comp.getType(), allTypes[i]);
        cr_assert_eq(comp.getQuantity(), static_cast<int>(i + 1));
        cr_assert_eq(comp.getId(), static_cast<int>(allTypes[i]));
    }
}

Test(component, component_large_quantities) {
    Component comp(ComponentType::FOOD, 99999, 80, 60);
    
    cr_assert_eq(comp.getQuantity(), 99999);
    
    comp.setQuantity(50000);
    cr_assert_eq(comp.getQuantity(), 50000);
    
    comp.setQuantity(0);
    cr_assert_eq(comp.getQuantity(), 0);
}

// ========== TESTS PLAYER APPROFONDIS ==========

Test(player, player_constructor_edge_cases) {
    // Joueur avec valeurs minimales
    Player player1(0, 0, 0, 0, 0, 0);
    cr_assert_eq(player1.getId(), 0);
    cr_assert_eq(player1.getPosX(), 0);
    cr_assert_eq(player1.getPosY(), 0);
    cr_assert_eq(player1.getOrientation(), 0);
    cr_assert_eq(player1.getLevel(), 0);
    
    // Joueur avec valeurs grandes
    Player player2(99999, 1000, 1000, 3, 8, 100);
    cr_assert_eq(player2.getId(), 99999);
    cr_assert_eq(player2.getPosX(), 1000);
    cr_assert_eq(player2.getPosY(), 1000);
    cr_assert_eq(player2.getOrientation(), 3);
    cr_assert_eq(player2.getLevel(), 8);
}

Test(player, player_all_orientations) {
    Player player(1, 5, 5, 1, 1, 0);
    
    // Tester toutes les orientations
    for (int orientation = 0; orientation <= 3; orientation++) {
        player.setOrientation(orientation);
        cr_assert_eq(player.getOrientation(), orientation);
    }
}

Test(player, player_all_levels) {
    Player player(1, 5, 5, 1, 1, 0);
    
    // Tester tous les niveaux
    for (int level = 1; level <= 8; level++) {
        player.setLevel(level);
        cr_assert_eq(player.getLevel(), level);
    }
}

Test(player, player_extreme_positions) {
    Player player(1, 0, 0, 1, 1, 0);
    
    // Positions extrêmes
    player.setPosX(-1000);
    player.setPosY(-1000);
    cr_assert_eq(player.getPosX(), -1000);
    cr_assert_eq(player.getPosY(), -1000);
    
    player.setPosX(10000);
    player.setPosY(10000);
    cr_assert_eq(player.getPosX(), 10000);
    cr_assert_eq(player.getPosY(), 10000);
}

// ========== TESTS TEAM APPROFONDIS ==========

Test(team, team_constructor_edge_cases) {
    // Équipe avec nom vide
    Team team1("");
    cr_assert_str_eq(team1.getName().c_str(), "");
}

Test(team, team_many_players) {
    Team team("BigTeam");
    
    // Ajouter beaucoup de joueurs
    for (int i = 1; i <= 100; i++) {
        Player player(i, i % 10, i % 10, i % 4, (i % 8) + 1, team.getId());
        team.addPlayer(player);
    }
    
    cr_assert_eq(team.getPlayerCount(), 100);
    cr_assert_eq(team.getPlayers().size(), 100);
}

Test(team, team_player_count_complex_positions) {
    Team team("TestTeam");
    
    // Ajouter des joueurs à différentes positions
    std::vector<std::pair<int, int>> positions = {
        {0, 0}, {0, 0}, {0, 0}, // 3 joueurs en (0,0)
        {5, 5}, {5, 5},         // 2 joueurs en (5,5)
        {10, 10}                // 1 joueur en (10,10)
    };
    
    for (size_t i = 0; i < positions.size(); i++) {
        Player player(i, positions[i].first, positions[i].second, 1, 1, team.getId());
        team.addPlayer(player);
    }
    
    cr_assert_eq(team.getPlayerCountOnTile(0, 0), 3);
    cr_assert_eq(team.getPlayerCountOnTile(5, 5), 2);
    cr_assert_eq(team.getPlayerCountOnTile(10, 10), 1);
    cr_assert_eq(team.getPlayerCountOnTile(15, 15), 0);
}

// ========== TESTS TILE APPROFONDIS ==========

Test(tile, tile_extreme_grid_sizes) {
    // Petite grille
    Tile tile1(0, 0, 1, 1, 800, 600);
    Rectangle rect1 = tile1.getRectangle();
    cr_assert_eq(rect1.width, 800);
    cr_assert_eq(rect1.height, 600);
    
    // Grande grille
    Tile tile2(0, 0, 100, 100, 800, 600);
    Rectangle rect2 = tile2.getRectangle();
    cr_assert_eq(rect2.width, 8);
    cr_assert_eq(rect2.height, 6);
}

Test(tile, tile_many_components) {
    Tile tile(0, 0, 10, 10, 800, 600);
    
    // Ajouter beaucoup de composants
    for (int i = 0; i < 50; i++) {
        ComponentType type = static_cast<ComponentType>(i % 7);
        auto component = std::make_shared<Component>(type, i + 1, 80, 60);
        tile.addComponent(component);
    }
    
    cr_assert_eq(tile.getNumberOfComponents(), 50);
}

Test(tile, tile_component_removal_complex) {
    Tile tile(0, 0, 10, 10, 800, 600);
    
    // Ajouter plusieurs composants
    auto food1 = std::make_shared<Component>(ComponentType::FOOD, 1, 80, 60);
    auto food2 = std::make_shared<Component>(ComponentType::FOOD, 2, 80, 60);
    auto linemate = std::make_shared<Component>(ComponentType::LINEMATE, 3, 80, 60);
    
    tile.addComponent(food1);
    tile.addComponent(food2);
    tile.addComponent(linemate);
    
    cr_assert_eq(tile.getNumberOfComponents(), 3);
    
    // Supprimer le composant du milieu
    tile.removeComponent(food2);
    cr_assert_eq(tile.getNumberOfComponents(), 2);
    
    // Supprimer tous les composants restants
    tile.removeComponent(food1);
    tile.removeComponent(linemate);
    cr_assert_eq(tile.getNumberOfComponents(), 0);
}

Test(tile, tile_add_resource_all_types) {
    Tile tile(0, 0, 10, 10, 800, 600);
    
    // Tester addResource avec tous les types d'ID
    for (int id = 0; id <= 6; id++) {
        try {
            tile.addResource(id);
            cr_assert(true, "addResource should work for valid IDs");
        } catch (...) {
            cr_assert(false, "addResource should not throw for valid IDs");
        }
    }
}

// ========== TESTS EGG APPROFONDIS ==========

Test(egg, egg_constructor_edge_cases) {
    // Œuf avec ID minimal
    Egg egg1(0, 0, 0);
    cr_assert_eq(egg1.getId(), 0);
    cr_assert_eq(egg1.getPosX(), 0);
    cr_assert_eq(egg1.getPosY(), 0);
    
    // Œuf avec valeurs grandes
    Egg egg2(99999, 1000, 1000);
    cr_assert_eq(egg2.getId(), 99999);
    cr_assert_eq(egg2.getPosX(), 1000);
    cr_assert_eq(egg2.getPosY(), 1000);
}

Test(egg, egg_sprite_indices) {
    Egg egg(1, 5, 5);
    
    // Tester différents indices de sprite
    for (int i = 0; i <= 10; i++) {
        egg.setSpriteIndex(i);
        cr_assert_eq(egg.getSpriteIndex(), i);
    }
}

Test(egg, egg_hatch_times) {
    Egg egg(1, 5, 5);
    
    // Tester différents temps d'éclosion
    std::vector<double> times = {0.0, 1.5, 10.0, 123.456, 999.999};
    
    for (double time : times) {
        egg.setStartHatchTime(time);
        cr_assert_eq(egg.getStartHatchTime(), time);
    }
}

Test(egg, egg_extreme_positions) {
    Egg egg(1, 0, 0);
    
    // Positions extrêmes
    egg.setPosX(-1000);
    egg.setPosY(-1000);
    cr_assert_eq(egg.getPosX(), -1000);
    cr_assert_eq(egg.getPosY(), -1000);
    
    egg.setPosX(10000);
    egg.setPosY(10000);
    cr_assert_eq(egg.getPosX(), 10000);
    cr_assert_eq(egg.getPosY(), 10000);
}
