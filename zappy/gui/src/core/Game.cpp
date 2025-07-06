/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Game
*/

#include "core/Game.hpp"
#include "utils/CustomRayLib.hpp"
#include "network/NetworkClient.hpp"
#include "network/PacketParser.hpp"

Game::Game(int isIpSet) {
  CustomRayLib::initWindow(windowWidth, windowHeight, "Zappy");
  CustomRayLib::setTargetFPS(60);

  status = isIpSet;
  elevations = std::make_unique<Elevation>();
  if (isIpSet)
    networkClient = std::make_unique<NetworkClient>();
}

Game::~Game() {
  networkClient.reset();
  packetParser.reset();
  CustomRayLib::closeWindow();
  tiles.clear();
}

void Game::run() {
  while (!CustomRayLib::windowShouldClose()) {
    update();
    draw();
  }
}

void Game::createTeam(const std::string &name) {
  auto team = std::make_unique<Team>(name);
  std::cout << "[DEBUG] Team created: " << name << " with id " << team->getId() << std::endl;
  teams.push_back(std::move(team));
}

void Game::initializeGrid(int x, int y) {
  sizeX = x;
  sizeY = y;

  for (int i = 0; i < sizeX; ++i) {
    for (int j = 0; j < sizeY; ++j) {
      auto tile = std::make_unique<Tile>(i, j, sizeX, sizeY, windowWidth, windowHeight);
      tiles.push_back(std::move(tile));
    }
  }
}

void Game::fillTileMap(int x, int y, int nbFood, int nbLinemate, int nbDeraumere, int nbSibur, int nbMendiane, int nbPhiras, int nbThystame) {
  for (auto &tile : tiles) {
    if (tile->getX() == x && tile->getY() == y) {
      std::cout << "[DEBUG] Filling tile at (" << tile->getX() << ", " << tile->getY() << ") with resources." << std::endl;
      
      auto addOrUpdateComponent = [&tile](ComponentType type, int quantity) {
        if (quantity <= 0) return;
        auto &components = tile->getComponents();
        for (auto &component : components) {
          if (component->getType() == type) {
            component->setQuantity(quantity);
            std::cout << "[DEBUG] Updated existing component " << (int)type << " quantity to " << quantity << std::endl;
            return;
          }
        }
        tile->addComponent(std::make_shared<Component>(type, quantity, tile->getSizeX(), tile->getSizeY()));
        std::cout << "[DEBUG] Created new component " << (int)type << " with quantity " << quantity << std::endl;
      };
      addOrUpdateComponent(ComponentType::FOOD, nbFood);
      addOrUpdateComponent(ComponentType::LINEMATE, nbLinemate);
      addOrUpdateComponent(ComponentType::DERAUMERE, nbDeraumere);
      addOrUpdateComponent(ComponentType::SIBUR, nbSibur);
      addOrUpdateComponent(ComponentType::MENDIANE, nbMendiane);
      addOrUpdateComponent(ComponentType::PHIRAS, nbPhiras);
      addOrUpdateComponent(ComponentType::THYSTAME, nbThystame);
      return;
    }
  }
}

void Game::createPlayer(int id, int x, int y, int orientation, int level, const std::string &teamName) {
  for (const auto &team : teams) {
    if (team->getName() == teamName) {
      auto player = std::make_unique<Player>(id, x, y, orientation, level, team->getId());
      team->addPlayer(*player);
      std::cout << "[DEBUG] Player created: ID=" << id << ", Team=" << teamName << std::endl;
      return;
    }
  }
  std::cerr << "[ERROR] Team not found: " << teamName << std::endl;
}

void Game::updatePlayer(int id, int x, int y, int orientation, int level) {
  for (const auto &team : teams) {
    for (const auto &player : team->getPlayers()) {
      if (player->getId() == id) {
        if (x >= 0)
          player->setPosX(x);
        if (y >= 0)
          player->setPosY(y);
        if (orientation > 0)
          player->setOrientation(orientation);
        if (level > 0)
          player->setLevel(level);
        std::cout << "[DEBUG] Player updated: ID=" << id << ", New Position=(" << x << ", " << y << "), Orientation=" << orientation << ", Level=" << level << std::endl;
        return;
      }
    }
  }
  std::cerr << "[ERROR] Player not found: ID=" << id << std::endl;
}

void Game::updateInventoryPlayer(int id, int q0, int q1, int q2, int q3, int q4, int q5, int q6) {
  for (const auto &team : teams) {
    for (const auto &player : team->getPlayers()) {
      if (player->getId() == id) {
        player->getInventory().clear();
        if (q0 > 0)
          player->getInventory().push_back(std::make_shared<Component>(ComponentType::FOOD, q0, -1, -1));
        if (q1 > 0)
          player->getInventory().push_back(std::make_shared<Component>(ComponentType::LINEMATE, q1, -1, -1));
        if (q2 > 0)
          player->getInventory().push_back(std::make_shared<Component>(ComponentType::DERAUMERE, q2, -1, -1));
        if (q3 > 0)
          player->getInventory().push_back(std::make_shared<Component>(ComponentType::SIBUR, q3, -1, -1));
        if (q4 > 0)
          player->getInventory().push_back(std::make_shared<Component>(ComponentType::MENDIANE, q4, -1, -1));
        if (q5 > 0)
          player->getInventory().push_back(std::make_shared<Component>(ComponentType::PHIRAS, q5, -1, -1));
        if (q6 > 0)
          player->getInventory().push_back(std::make_shared<Component>(ComponentType::THYSTAME, q6, -1, -1));
        std::cout << "[DEBUG] Inventory updated for Player ID=" << id << std::endl;
        return;
      }
    }
  }
  std::cerr << "[ERROR] Player not found: ID=" << id << std::endl;
}

void Game::removePlayer(int id) {
  for (auto &team : teams) {
    auto &players = team->getPlayers();
    auto it = std::remove_if(players.begin(), players.end(),
      [id](const std::unique_ptr<Player> &player) { return player->getId() == id; });
    if (it != players.end()) {
      players.erase(it, players.end());
      std::cout << "[DEBUG] Player removed: ID=" << id << std::endl;
      return;
    }
  }
  std::cerr << "[ERROR] Player not found: ID=" << id << std::endl;
}

void Game::createEgg(int playerId, int id, int posX, int posY) {
  for (const auto &team : teams) {
    for (const auto &player : team->getPlayers()) {
      if (player->getId() == playerId) {
        auto egg = std::make_unique<Egg>(id, posX, posY);
        player->addEgg(std::move(egg));
        std::cout << "[DEBUG] Egg created: ID=" << id << ", Player ID=" << playerId << ", Position=(" << posX << ", " << posY << ")" << std::endl;
        return;
      }
    }
  }
  std::cerr << "[ERROR] Player not found for egg creation: Player ID=" << playerId << std::endl;
}

void Game::updateEgg(int id, bool isHatching) {
  for (const auto &team : teams) {
    for (const auto &player : team->getPlayers()) {
      for (const auto &egg : player->getEggs()) {
        if (egg->getId() == id) {
          egg->setHatching(isHatching);
          egg->setStartHatchTime(CustomRayLib::getTime());
          if (!isHatching)
            egg->setSpriteIndex(6);
          std::cout << "[DEBUG] Egg updated: ID=" << id << ", Hatching=" << isHatching << std::endl;
          return;
        }
      }
    }
  }
  std::cerr << "[ERROR] Egg not found: ID=" << id << std::endl;
}

void Game::removeEgg(int id) {
  for (const auto &team : teams) {
    for (const auto &player : team->getPlayers()) {
      auto &eggs = player->getEggs();
      auto it = std::remove_if(eggs.begin(), eggs.end(),
        [id](const std::shared_ptr<Egg> &egg) { return egg->getId() == id; });
      if (it != eggs.end()) {
        eggs.erase(it, eggs.end());
        std::cout << "[DEBUG] Egg removed: ID=" << id << std::endl;
        return;
      }
    }
  }
  std::cerr << "[ERROR] Egg not found: ID=" << id << std::endl;
}

void Game::newElevation(int x, int y, int level, const std::vector<int> ids) {
  elevations->initElevation(x, y, level, ids);
  std::cout << "[DEBUG] New elevation created at Position=(" << x << ", " << y << "), Level=" << level << std::endl;
}

void Game::updateElevation(int x, int y, int success) {
  if (success == 1) {
    elevations->setElevationSuccess(true);
    elevations->setStartTime(CustomRayLib::getTime());
    elevations->setSpriteIndex(0);
  } else {
    for (const auto &team : teams) {
      for (const auto &player : team->getPlayers()) {
        if (player->getPosX() == x && player->getPosY() == y) {
          player->setSpriteIndex(0);
          player->setStartAnimationTime(-1.0);
          elevations->setElevationActive(false);
        }
      }
    }
  }
}

void Game::removeComponentId(int id, int playerId) {
  int playerX = -1, playerY = -1;
  bool playerFound = false;

  for (const auto &team : teams) {
    for (const auto &player : team->getPlayers()) {
      if (player->getId() == playerId) {
        playerX = player->getPosX();
        playerY = player->getPosY();
        playerFound = true;
        break;
      }
    }
    if (playerFound) break;
  }
  
  if (!playerFound) {
    std::cerr << "[ERROR] Player not found: ID=" << playerId << std::endl;
    return;
  }
  
  for (auto &tile : tiles) {
    if (tile->getX() == playerX && tile->getY() == playerY) {
      auto &components = tile->getComponents();
      
      for (auto it = components.begin(); it != components.end(); ++it) {
        if ((*it)->getId() == id + 1) {
          int currentQuantity = (*it)->getQuantity();
          std::cout << "[DEBUG] Found component ID=" << (id + 1) << " with quantity=" << currentQuantity << std::endl;
          
          if (currentQuantity <= 1) {
            components.erase(it);
            std::cout << "[DEBUG] Component removed completely: ID=" << (id + 1) << " from tile (" << playerX << ", " << playerY << ")" << std::endl;
          } else {
            (*it)->setQuantity(currentQuantity - 1);
            std::cout << "[DEBUG] Component quantity decreased: ID=" << (id + 1) << ", new quantity=" << (currentQuantity - 1) << std::endl;
          }
          return;
        }
      }
      break;
    }
  }
  std::cerr << "[ERROR] Component not found: ID=" << id << " on player's tile (" << playerX << ", " << playerY << ")" << std::endl;
}

void Game::addResourceToTile(int playerId, int resourceId)
{
  for (const auto &team : teams) {
    for (const auto &player : team->getPlayers()) {
      if (player->getId() == playerId) {
        int tileX = player->getPosX();
        int tileY = player->getPosY();
        
        for (auto &tile : tiles) {
          if (tile->getX() == tileX && tile->getY() == tileY) {
            tile->addResource(resourceId);
            std::cout << "[DEBUG] Resource added to tile: Player ID=" << playerId << ", Resource ID=" << resourceId << ", Position=(" << tileX << ", " << tileY << ")" << std::endl;
            return;
          }
        }
        std::cerr << "[ERROR] Tile not found at position (" << tileX << ", " << tileY << ")" << std::endl;
        return;
      }
    }
  }
  std::cerr << "[ERROR] Player not found for adding resource: Player ID=" << playerId << std::endl;
}

void Game::removeResourcesFromTile(int x, int y, int linemate, int deraumere, int sibur, int mendiane, int phiras, int thystame) {
  // Trouver la tile correspondante
  for (auto &tile : tiles) {
    if (tile->getX() == x && tile->getY() == y) {
      auto &components = tile->getComponents();
      std::cout << "[DEBUG] Removing resources from tile (" << x << ", " << y << ")" << std::endl;
      
      // Fonction helper pour supprimer une quantité d'un type de composant
      auto removeComponentType = [&components, x, y](ComponentType type, int quantityToRemove) {
        if (quantityToRemove <= 0) return;
        
        for (auto it = components.begin(); it != components.end(); ++it) {
          if ((*it)->getType() == type) {
            int currentQuantity = (*it)->getQuantity();
            std::cout << "[DEBUG] Found " << (int)type << " with quantity " << currentQuantity << ", removing " << quantityToRemove << std::endl;
            
            if (currentQuantity <= quantityToRemove) {
              // Supprimer complètement le composant
              components.erase(it);
              std::cout << "[DEBUG] Component " << (int)type << " completely removed" << std::endl;
            } else {
              // Diminuer la quantité
              (*it)->setQuantity(currentQuantity - quantityToRemove);
              std::cout << "[DEBUG] Component " << (int)type << " quantity updated to " << (currentQuantity - quantityToRemove) << std::endl;
            }
            return;
          }
        }
        std::cerr << "[WARNING] Component type " << (int)type << " not found on tile (" << x << ", " << y << ")" << std::endl;
      };
      
      // Supprimer chaque type de ressource
      removeComponentType(ComponentType::LINEMATE, linemate);
      removeComponentType(ComponentType::DERAUMERE, deraumere);
      removeComponentType(ComponentType::SIBUR, sibur);
      removeComponentType(ComponentType::MENDIANE, mendiane);
      removeComponentType(ComponentType::PHIRAS, phiras);
      removeComponentType(ComponentType::THYSTAME, thystame);
      
      std::cout << "[DEBUG] Resources removal completed for tile (" << x << ", " << y << ")" << std::endl;
      return;
    }
  }
  std::cerr << "[ERROR] Tile not found at position (" << x << ", " << y << ")" << std::endl;
}
