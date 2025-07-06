/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Game
*/

#pragma once

#include "core/Team.hpp"
#include "core/Player.hpp"
#include "core/Tile.hpp"
#include "core/Elevation.hpp"
#include "utils/CustomRayLib.hpp"

#include <string>
#include <vector>
#include <iostream>
#include <memory>

// Déclarations anticipées pour éviter les inclusions circulaires
class Team;
class Elevation;
class Player;
class NetworkClient;
class PacketParser;

class Game {
public:
  Game(int isIpSet = 0);
  ~Game();

  /**
   * @brief Gets the Width of the game window.
   *
   * @return The Width of the game window.
   */
  int getwindowWidth() const { return windowWidth; }

  /**
   * @brief Gets the Height of the game window.
   *
   * @return The Height of the game window.
   */
  int getwindowHeight() const { return windowHeight; }

  /**
   * @brief Draws the game elements.
   */
  void draw();
  /**
   * @brief Runs the game loop.
   */
  void run();
  /**
   * @brief Updates the game state.
   */
  void update();

  /**
   * @brief Draws the grid for the game window.
   */
  void drawGrid();

  /**
   * @brief Draws the components on the tile.
   *
   * @param components The components to draw.
   * @param tileRect The rectangle of the tile.
   */
  void drawComponents(const std::vector<std::shared_ptr<Component>> &components, const Rectangle &tileRect, int playerCountOnTile);
  /**
   * @brief Draws players in their respective teams.
   */
  void drawPlayersInTeams(int tileX, int tileY, const Rectangle &tileRect, int componentsSize);

  /**
    * @brief Get Rectangle for player's sprite.
    */
  Rectangle getPlayerRectangle(Player &player);

  /**
   * @brief Draws elevations on the tile.
   *
   * @param tileX The X position of the tile.
   * @param tileY The Y position of the tile.
   * @param tileRect The rectangle of the tile.
   */
  void drawElevations(int tileX, int tileY, const Rectangle &tileRect);

  /**
   * @brief Initializes the grid with tiles based on the game size.
   *
   * This function creates a grid of rectangles that represent the tiles in the game window.
   */
  void initializeGrid(int x, int y);
  
  /**
   * @brief Fills the tile map with the specified parameters.
   */
  void fillTileMap(int, int, int, int, int, int, int, int, int);

  /**
   * @brief Creates a new team with the specified name.
   *
   * @param name The name of the team to create.
   */
  void createTeam(const std::string &name);

  /**
   * @brief Creates a player with the specified parameters.
   *
   * @param id The ID of the player.
   * @param x The X position of the player.
   * @param y The Y position of the player.
   * @param orientation The orientation of the player.
   * @param level The level of the player.
   * @param teamName The name of the team the player belongs to.
   */
  void createPlayer(int id, int x, int y, int orientation, int level, const std::string &teamName);

  /**
   * @brief Updates the player with the specified parameters.
   *
   * @param id The ID of the player to update.
   * @param x The new X position of the player.
   * @param y The new Y position of the player.
   * @param orientation The new orientation of the player.
   * @param level The new level of the player.
   */
  void updatePlayer(int id, int x, int y, int orientation, int level);

  /**
   * @brief Updates the inventory of the player with the specified parameters.
   *
   * @param id The ID of the player to update.
   * @param q0 The quantity of food.
   * @param q1 The quantity of linemate.
   * @param q2 The quantity of deraumere.
   * @param q3 The quantity of sibur.
   * @param q4 The quantity of mendiane.
   * @param q5 The quantity of phiras.
   * @param q6 The quantity of thystame.
   */
  void updateInventoryPlayer(int id, int q0, int q1, int q2, int q3, int q4, int q5, int q6);

  /**
   * @brief Removes a player from the game.
   *
   * @param id The ID of the player to remove.
   */
  void removePlayer(int id);

  /**
   * @brief Create a egg
   * 
   * @param 
   */
  void createEgg(int playerId, int id, int posX, int posY);

  /**
   * @brief Updates the egg with the specified parameters.
   *
   * @param id The ID of the egg to update.
   * @param isHatching The hatching state of the egg.
   */
  void updateEgg(int id, bool isHatching);

  /**
   * @brief Removes an egg from the game.
   *
   * @param id The ID of the egg to remove.
   */
  void removeEgg(int id);

  /**
   * @brief Updates the elevation of a tile with the specified parameters.
   *
   * @param x The X position of the tile.
   * @param y The Y position of the tile.
   * @param level The new level of the tile.
   * @param ids The player IDs on the tile.
   */
  void newElevation(int x, int y, int level, std::vector<int> ids);

  /**
   * @brief Updates the elevation of a tile.
   *
   * @param x The X position of the tile.
   * @param y The Y position of the tile.
   * @param success The success state of the elevation.
   */
  void updateElevation(int x, int y, int success);

  /**
   * @brief Get the status flag.
   * @return The state of the status flag.
   */
  int getServerStatus() const { return status; }

  /**
   * @brief Set the status flag.
   * @param isSet The state to set for the status flag.
   */
  void setServerStatus(int isSet) { status = isSet; }

  /**
   * @brief setThe Gui is listening for server messages.
   * @param isListening The state of the GUI listening flag.
   */
  bool isGuiListening() const { return guiIsListening; }
  /**
   * @brief Sets the GUI listening state.
   * @param isListening The state to set for the GUI listening flag.
   */
  void setGuiIsListening(bool isListening) { guiIsListening = isListening; }

  /**
   * @brief Gets the server IP address.
   *
   * @return The server IP address.
   */
  std::string getServerIp() const { return serverIp; }

  /**
   * @brief Sets the server IP address.
   *
   * @param ip The server IP address to set.
   */
  void setServerIp(const std::string &ip) { serverIp = ip; }

  /**
   * @brief Gets the server port.
   *
   * @return The server port.
   */
  std::string getServerPort() const { return serverPort; }

  /**
   * @brief Sets the server port.
   *
   * @param port The server port to set.
   */
  void setServerPort(const std::string &port) { serverPort = port; }

  /**
   * @brief Initialise la connexion au serveur avec les options spécifiées
   * 
   * @param argv Arguments de la ligne de commande
   */
  void initServerConnection(char **argv);

  /**
   * @brief Initialise la connexion au serveur par l'interface graphique
   */
  void initServerConnectionByGUI();

  /**
   * @brief Lance l'écoute du serveur
   */
  void listenToServer();

  /**
   * @brief Gets the disconnect timestamp.
   *
   * @return The timestamp of the disconnection.
   */
  void setWinningTeam(const std::string &teamName) { winningTeam = teamName; }

  /**
   * @brief Remove a component ID from the player.
   *
   * @param id The ID of the component to remove.
   */
  void removeComponentId(int id, int playerId);

  /**
   * @brief Adds a resource to a tile.
   *
   * @param playerId The ID of the player placing the resource.
   * @param resourceId The ID of the resource to place.
   */
  void addResourceToTile(int playerId, int resourceId);

  /**
   * @brief Remove resources from a specific tile.
   *
   * @param x The x coordinate of the tile.
   * @param y The y coordinate of the tile.
   * @param linemate Number of linemate to remove.
   * @param deraumere Number of deraumere to remove.
   * @param sibur Number of sibur to remove.
   * @param mendiane Number of mendiane to remove.
   * @param phiras Number of phiras to remove.
   * @param thystame Number of thystame to remove.
   */
  void removeResourcesFromTile(int x, int y, int linemate, int deraumere, int sibur, int mendiane, int phiras, int thystame);

  // Timestamp for disconnecting
  float disconnectTimestamp = 0.0f;
  // Winning team name
  std::string winningTeam = "";

private:
  // Membres de base du jeu
  int windowWidth = 800; // Width of the game window
  int windowHeight = 600; // Height of the game window
  int sizeX = 10; // Size X, corresponds to the nb of tiles in the game window
  int sizeY = 10; // Size Y, corresponds to the nb of tiles in the game window
  std::vector<std::unique_ptr<Tile>> tiles; // List of rectangles representing the tiles in the game window (using unique_ptr for memory management)
  std::vector<std::unique_ptr<Team>> teams; // List of teams in the game (using unique_ptr for memory management)
  std::unique_ptr<Elevation> elevations; // Elevation object for the game
  int posX = 0;
  int posY = 0;
  
  // Elements for no args start
  int status = 0;
  bool guiIsListening = false;
  std::string serverIp;
  std::string serverPort;
  std::unique_ptr<NetworkClient> networkClient;
  std::unique_ptr<PacketParser> packetParser;
  bool ipBoxActive = false;
  bool portBoxActive = false;
  std::string ipInput = "";
  std::string portInput = "";
  static const int MAX_INPUT_CHARS = 32;

  // Gerer UI -- Server Input
  void drawServerInputUI();
  void drawMainPanel(int centerX, int centerY);
  void drawTitle(int centerX, int centerY);
  void drawInputBoxes(int centerX, int centerY);
  void handleInputBoxClicks(const Rectangle& ipBox, const Rectangle& portBox);
  void drawIPBox(const Rectangle& ipBox);
  void drawPortBox(const Rectangle& portBox);
  void drawInputLabels(const Rectangle& ipBox, const Rectangle& portBox);
  void drawInputContent(const Rectangle& ipBox, const Rectangle& portBox);
  void drawConnectButton(int centerX, int centerY);
  void drawInstructions(int centerX, int centerY);
  void handleConnectionAttempt(int centerX, int centerY);
  void drawDisconnectedUI();
  void drawEndOfTheGameUI();
  bool isValidIP(const std::string& ip);
  bool isValidPort(const std::string& port);
  void handleTextInput();
  // Gerer UI -- Disconnected UI
  void drawDisconnectedBackground(float time);
  void drawDisconnectedPanel(float time, int centerX, int centerY);
  void drawDisconnectedBorder(float time, const Rectangle& mainPanel);
  void drawDisconnectedIcon(int centerX, int centerY);
  void drawDisconnectedTitle(float time, int centerX, int centerY);
  void drawDisconnectedMessages(int centerX, int centerY);
  void drawDisconnectedButton(float time, int centerX, int centerY);
  void drawDisconnectedButtonGlow(float time, const Rectangle& button);
  void drawDisconnectedButtonText(const Rectangle& button);
  void handleDisconnectedButtonClick(bool isHovering);
  void drawDisconnectionTime(int centerX, int centerY);
  // Gerer UI -- End of the Game UI
  void drawEndGameBackground(float time);
  void drawEndGameConfetti(float time);
  Color getConfettiColor(int index, float time);
  void drawEndGamePanel(float time, int centerX, int centerY);
  void drawEndGameGoldenBorder(float time, const Rectangle& mainPanel);
  void drawEndGameTrophy(float time, int centerX, int centerY);
  void drawTrophyBase(float trophyBaseX, float trophyBaseY);
  void drawTrophyCup(float trophyBaseX, float trophyBaseY, float trophySize);
  void drawTrophyHandles(float trophyBaseX, float trophyBaseY, float trophySize);
  void drawTrophySparkles(float time, float trophyBaseX, float trophyBaseY, float trophySize);
  void drawEndGameTitle(float time, int centerX, int centerY);
  Color getTitleColor1(float time);
  Color getTitleColor2(float time);
  void drawEndGameWinner(float time, int centerX, int centerY);
  void drawWinnerTeamName(float time, int centerX, int centerY, const std::string& winnerTeam);
  void drawEndGameButton(float time, int centerX, int centerY);
  void drawEndGameButtonGlow(float time, const Rectangle& menuButton);
  void drawEndGameButtonText(const Rectangle& menuButton);
  void handleEndGameButtonClick(bool isHovering);
};
