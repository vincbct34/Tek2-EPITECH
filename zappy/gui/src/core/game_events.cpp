/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** game_events
*/

#include "Game.hpp"
#include "network/NetworkClient.hpp"
#include "network/PacketParser.hpp"
#include <cmath>

void Game::update() {
  if (!isGuiListening() && getServerStatus() == 1) {
    std::cout << "[DEBUG] GUI is now listening for server messages." << std::endl;
    
    try {
      initServerConnectionByGUI();
      listenToServer();
      setGuiIsListening(true);
    } catch (const std::exception& e) {
      std::cerr << "[ERROR] Failed to connect: " << e.what() << std::endl;
      networkClient.reset();
      packetParser.reset();
      setServerStatus(4);
    }
  }
}

void Game::drawGrid() {
  for (const auto &tile : tiles) {
    Rectangle rect = tile->getRectangle();
    CustomRayLib::drawRectangleRec(rect, RAYWHITE);
    CustomRayLib::drawRectangleLines(rect.x, rect.y, rect.width, rect.height, BLACK);

    int playerCountOnTile = 0;
    for (const auto &team : teams)
      playerCountOnTile += team->getPlayerCountOnTile(tile->getX(), tile->getY());
    drawComponents(tile->getComponents(), rect, playerCountOnTile);
    drawPlayersInTeams(tile->getX(), tile->getY(), rect, tile->getComponents().size());
    drawElevations(tile->getX(), tile->getY(), rect);
  }
}

void Game::draw() {
  CustomRayLib::beginDrawing();
  CustomRayLib::clearBackground(RAYWHITE);
  int status = getServerStatus();

  if (status == 0) {
    drawServerInputUI();
  } else if (status == 1) {
    drawGrid();
  } else if (status == 3) {
    drawEndOfTheGameUI();
  } else if (status == 4) {
    drawDisconnectedUI();
  }
  CustomRayLib::endDrawing();
}

void Game::handleTextInput() {
  int key = CustomRayLib::getCharPressed();
  
  while (key > 0) {
    if (ipBoxActive && ipInput.length() < MAX_INPUT_CHARS) {
      if ((key >= 48 && key <= 57) || key == 46) {
        ipInput += static_cast<char>(key);
      }
    }
    if (portBoxActive && portInput.length() < MAX_INPUT_CHARS) {
      if (key >= 48 && key <= 57) { // Only numbers
        portInput += static_cast<char>(key);
      }
    }
    key = CustomRayLib::getCharPressed();
  }
  if (CustomRayLib::isKeyPressed(KEY_BACKSPACE)) {
    if (ipBoxActive && !ipInput.empty()) {
      ipInput.pop_back();
    }
    if (portBoxActive && !portInput.empty()) {
      portInput.pop_back();
    }
  }
}

bool Game::isValidIP(const std::string& ip) {
  if (ip.empty()) return false;
  int dots = 0;

  for (char c : ip) {
    if (c == '.') {
      dots++;
    } else if (c < '0' || c > '9') {
      return false;
    }
  }
  return dots == 3 && ip.length() >= 7;
}

bool Game::isValidPort(const std::string& port) {
  if (port.empty()) return false;
  
  try {
    int portNum = std::stoi(port);
    return portNum > 0 && portNum <= 65535;
  } catch (...) {
    return false;
  }
}

/*void Game::drawServerInputUI() {
  CustomRayLib::clearBackground(Color{30, 30, 40, 255});
  
  // Calculer les positions au centre de l'écran
  int centerX = windowWidth / 2;
  int centerY = windowHeight / 2;
  int boxWidth = 350;
  int boxHeight = 55;
  int spacing = 25;
  
  // Panel principal avec ombre (encore plus agrandi)
  Rectangle mainPanel = {
    static_cast<float>(centerX - boxWidth / 2 - 60),
    static_cast<float>(centerY - 200),
    static_cast<float>(boxWidth + 120),
    static_cast<float>(420)
  };
  
  // Ombre du panel
  Rectangle shadowPanel = {
    mainPanel.x + 5,
    mainPanel.y + 5,
    mainPanel.width,
    mainPanel.height
  };
  CustomRayLib::drawRectangleRec(shadowPanel, Color{0, 0, 0, 50});
  
  // Panel principal
  CustomRayLib::drawRectangleRec(mainPanel, Color{50, 50, 60, 255});
  CustomRayLib::drawRectangleLines(mainPanel.x, mainPanel.y, mainPanel.width, mainPanel.height, Color{80, 80, 90, 255});
  
  // Rectangles pour les boxes avec coins arrondis simulés
  Rectangle ipBox = {
    static_cast<float>(centerX - boxWidth / 2),
    static_cast<float>(centerY - boxHeight - spacing / 2 + 20),
    static_cast<float>(boxWidth),
    static_cast<float>(boxHeight)
  };
  
  Rectangle portBox = {
    static_cast<float>(centerX - boxWidth / 2),
    static_cast<float>(centerY + spacing / 2 + 20),
    static_cast<float>(boxWidth),
    static_cast<float>(boxHeight)
  };
  
  // Titre principal avec style
  const char* title = "ZAPPY";
  const char* subtitle = "Server Connection";
  
  int titleWidth = CustomRayLib::measureText(title, 36);
  int subtitleWidth = CustomRayLib::measureText(subtitle, 18);
  
  // Titre avec effet de brillance
  CustomRayLib::drawText(title, centerX - titleWidth / 2 + 2, centerY - 130 + 2, 36, Color{0, 0, 0, 100}); // Ombre
  CustomRayLib::drawText(title, centerX - titleWidth / 2, centerY - 130, 36, Color{100, 200, 255, 255}); // Bleu clair
  
  CustomRayLib::drawText(subtitle, centerX - subtitleWidth / 2, centerY - 95, 18, Color{200, 200, 200, 255});
  
  // Ligne de séparation
  CustomRayLib::drawLine(centerX - 100, centerY - 75, centerX + 100, centerY - 75, Color{80, 80, 90, 255});
  
  // Gérer les clics de souris
  Vector2 mousePos = CustomRayLib::getMousePosition();
  if (CustomRayLib::isMouseButtonPressed(MOUSE_LEFT_BUTTON)) {
    ipBoxActive = CustomRayLib::checkCollisionPointRec(mousePos, ipBox);
    portBoxActive = CustomRayLib::checkCollisionPointRec(mousePos, portBox);
  }
  
  // Gérer l'input texte
  handleTextInput();
  
  // Couleurs pour les boxes
  Color ipBoxColor = ipBoxActive ? Color{70, 130, 180, 255} : Color{60, 60, 70, 255};
  Color ipBorderColor = ipBoxActive ? Color{100, 200, 255, 255} : Color{100, 100, 120, 255};
  Color portBoxColor = portBoxActive ? Color{70, 130, 180, 255} : Color{60, 60, 70, 255};
  Color portBorderColor = portBoxActive ? Color{100, 200, 255, 255} : Color{100, 100, 120, 255};
  
  // Dessiner les boxes IP avec effet de focus
  CustomRayLib::drawRectangleRec(ipBox, ipBoxColor);
  CustomRayLib::drawRectangleLines(ipBox.x, ipBox.y, ipBox.width, ipBox.height, ipBorderColor);
  
  // Effet de glow pour la box active
  if (ipBoxActive) {
    CustomRayLib::drawRectangleLines(ipBox.x - 1, ipBox.y - 1, ipBox.width + 2, ipBox.height + 2, 
                                     Color{100, 200, 255, 100});
  }
  
  // Dessiner les boxes Port
  CustomRayLib::drawRectangleRec(portBox, portBoxColor);
  CustomRayLib::drawRectangleLines(portBox.x, portBox.y, portBox.width, portBox.height, portBorderColor);
  
  if (portBoxActive) {
    CustomRayLib::drawRectangleLines(portBox.x - 1, portBox.y - 1, portBox.width + 2, portBox.height + 2, 
                                     Color{100, 200, 255, 100});
  }
  
  // Labels sans emojis (descendus)
  CustomRayLib::drawText("IP Address", ipBox.x, ipBox.y - 25, 16, Color{200, 200, 200, 255});
  CustomRayLib::drawText("Port", portBox.x, portBox.y - 25, 16, Color{200, 200, 200, 255});
  
  // Texte dans les boxes avec placeholder
  std::string ipDisplay = ipInput.empty() ? "127.0.0.1" : ipInput;
  std::string portDisplay = portInput.empty() ? "4242" : portInput;
  
  // Couleur du texte (plus claire pour les placeholders)
  Color ipTextColor = ipInput.empty() ? Color{150, 150, 150, 255} : Color{255, 255, 255, 255};
  Color portTextColor = portInput.empty() ? Color{150, 150, 150, 255} : Color{255, 255, 255, 255};
  
  // Ajouter curseur clignotant si la box est active
  if (ipBoxActive && ((int)(CustomRayLib::getTime() * 2) % 2)) {
    ipDisplay += "|";
    ipTextColor = Color{255, 255, 255, 255};
  }
  if (portBoxActive && ((int)(CustomRayLib::getTime() * 2) % 2)) {
    portDisplay += "|";
    portTextColor = Color{255, 255, 255, 255};
  }
  
  // Centrer le texte verticalement dans les boxes
  CustomRayLib::drawText(ipDisplay.c_str(), ipBox.x + 15, ipBox.y + 18, 18, ipTextColor);
  CustomRayLib::drawText(portDisplay.c_str(), portBox.x + 15, portBox.y + 18, 18, portTextColor);
  
  // Bouton de connexion stylisé (descendu)
  Rectangle connectButton = {
    static_cast<float>(centerX - 80),
    static_cast<float>(centerY + 100),
    160,
    45
  };
  
  bool isHovering = CustomRayLib::checkCollisionPointRec(mousePos, connectButton);
  Color buttonColor = isHovering ? Color{70, 130, 180, 255} : Color{50, 100, 150, 255};
  Color buttonBorder = isHovering ? Color{100, 200, 255, 255} : Color{80, 150, 200, 255};
  
  CustomRayLib::drawRectangleRec(connectButton, buttonColor);
  CustomRayLib::drawRectangleLines(connectButton.x, connectButton.y, connectButton.width, connectButton.height, buttonBorder);
  
  const char* buttonText = "CONNECT";
  int buttonTextWidth = CustomRayLib::measureText(buttonText, 16);
  CustomRayLib::drawText(buttonText, connectButton.x + (connectButton.width - buttonTextWidth) / 2, 
                        connectButton.y + 14, 16, Color{255, 255, 255, 255});
  
  // Instructions sans emojis (descendues)
  const char* instructions = "Press ENTER or click CONNECT to join";
  int instructionsWidth = CustomRayLib::measureText(instructions, 14);
  CustomRayLib::drawText(instructions, 
                        centerX - instructionsWidth / 2, centerY + 160, 14, Color{150, 150, 150, 255});
  
  // Indicateur de statut (descendu)
  const char* statusText = "Ready to connect";
  int statusWidth = CustomRayLib::measureText(statusText, 12);
  CustomRayLib::drawText(statusText, centerX - statusWidth / 2, centerY + 180, 12, Color{100, 200, 100, 255});
  
  // Vérifier si on peut valider (ENTER pressé ou bouton cliqué)
  bool shouldConnect = CustomRayLib::isKeyPressed(KEY_ENTER) || 
                      (CustomRayLib::isMouseButtonPressed(MOUSE_LEFT_BUTTON) && isHovering);
  
  if (shouldConnect) {
    std::string finalIP = ipInput.empty() ? "127.0.0.1" : ipInput;
    std::string finalPort = portInput.empty() ? "4242" : portInput;
    
    if (isValidIP(finalIP) && isValidPort(finalPort)) {
      setServerIp(finalIP);
      setServerPort(finalPort);
      // Ici renitialiser tous les std::vector<std::unique_ptr<Tile>> tiles;
      tiles.clear();
      // Réinitialiser les équipes et les joueurs si nécessaire
      teams.clear();
      
      setServerStatus(1);
      std::cout << "[DEBUG] Server info set: " << finalIP << ":" << finalPort << std::endl;
    } else {
      std::cout << "[ERROR] Invalid IP or Port format!" << std::endl;
    }
  }
}*/

/*void Game::drawDisconnectedUI() {
  // Dégradé de fond animé
  float time = (float)CustomRayLib::getTime() * 0.2f;
  Color bgColor1 = {30, 30, 40, 255};
  Color bgColor2 = {20, 20, 30, 255};
  
  for (int i = 0; i < windowHeight; i += 4) {
    float factor = (sinf(time + i * 0.01f) + 1.0f) * 0.5f;
    Color color = {
      (unsigned char)(bgColor1.r * factor + bgColor2.r * (1 - factor)),
      (unsigned char)(bgColor1.g * factor + bgColor2.g * (1 - factor)),
      (unsigned char)(bgColor1.b * factor + bgColor2.b * (1 - factor)),
      255
    };
    CustomRayLib::drawLine(0, i, windowWidth, i, color);
  }
  
  // Calculer les positions au centre de l'écran
  int centerX = windowWidth / 2;
  int centerY = windowHeight / 2;
  
  // Créer un effet d'impulsion pour le panel
  float pulse = sinf(time * 3.0f) * 0.05f + 1.0f;
  float panelWidth = 450 * pulse;
  float panelHeight = 280 * pulse;
  
  // Panel principal avec ombre
  Rectangle mainPanel = {
    static_cast<float>(centerX - panelWidth / 2),
    static_cast<float>(centerY - panelHeight / 2),
    panelWidth,
    panelHeight
  };
  
  // Ombre du panel
  Rectangle shadowPanel = {
    mainPanel.x + 8,
    mainPanel.y + 8,
    mainPanel.width,
    mainPanel.height
  };
  CustomRayLib::drawRectangleRec(shadowPanel, Color{0, 0, 0, 40});
  
  // Panel principal avec effet de dégradé
  CustomRayLib::drawRectangleRec(mainPanel, Color{40, 40, 55, 255});
  
  // Bordure animée
  Color borderColor1 = {180, 60, 60, 255}; // Rouge
  Color borderColor2 = {100, 30, 30, 255}; // Rouge foncé
  
  for (int i = 0; i < 3; i++) {
    float alpha = (sinf(time * 2.0f + i * 0.5f) + 1.0f) * 0.5f;
    Color borderColor = {
      (unsigned char)(borderColor1.r * alpha + borderColor2.r * (1 - alpha)),
      (unsigned char)(borderColor1.g * alpha + borderColor2.g * (1 - alpha)),
      (unsigned char)(borderColor1.b * alpha + borderColor2.b * (1 - alpha)),
      255
    };
    CustomRayLib::drawRectangleLines(
      mainPanel.x - i, mainPanel.y - i, 
      mainPanel.width + i * 2, mainPanel.height + i * 2, 
      borderColor
    );
  }
  
  // Icône d'alerte
  float iconSize = 64.0f;
  float iconY = centerY - iconSize - 15;
  
  // Dessin du cercle d'alerte
  CustomRayLib::drawCircle(centerX, iconY, iconSize/2 + 5, Color{80, 10, 10, 255});
  CustomRayLib::drawCircle(centerX, iconY, iconSize/2, Color{190, 30, 30, 255});
  
  // Point d'exclamation
  CustomRayLib::drawRectangle(centerX - 5, iconY - 20, 10, 30, WHITE);
  CustomRayLib::drawCircle(centerX, iconY + 20, 5, WHITE);
  
  // Titre avec effet de brillance
  const char* title = "CONNECTION LOST";
  float titleFontSize = 28;
  int titleWidth = CustomRayLib::measureText(title, titleFontSize);
  
  // Ombre du titre
  CustomRayLib::drawText(title, centerX - titleWidth / 2 + 2, centerY + 2, titleFontSize, Color{0, 0, 0, 100});
  
  // Titre avec effet de couleur animé
  float colorPulse = (sinf(time * 4.0f) + 1.0f) * 0.5f;
  Color titleColor = {
    (unsigned char)(255 * colorPulse + 200 * (1 - colorPulse)),
    (unsigned char)(80 * colorPulse + 40 * (1 - colorPulse)),
    (unsigned char)(80 * colorPulse + 40 * (1 - colorPulse)),
    255
  };
  CustomRayLib::drawText(title, centerX - titleWidth / 2, centerY, titleFontSize, titleColor);
  
  // Message d'erreur
  const char* message = "The server connection has been interrupted";
  int messageWidth = CustomRayLib::measureText(message, 18);
  CustomRayLib::drawText(message, centerX - messageWidth / 2, centerY + 40, 18, Color{220, 220, 220, 255});
  
  // Sous-message
  const char* submessage = "Please check your network and try reconnecting";
  int submessageWidth = CustomRayLib::measureText(submessage, 16);
  CustomRayLib::drawText(submessage, centerX - submessageWidth / 2, centerY + 70, 16, Color{180, 180, 180, 255});
  
  // Bouton de retour au menu
  Rectangle reconnectButton = {
    static_cast<float>(centerX - 100),
    static_cast<float>(centerY + 110),
    200,
    45
  };
  
  Vector2 mousePos = CustomRayLib::getMousePosition();
  bool isHovering = CustomRayLib::checkCollisionPointRec(mousePos, reconnectButton);
  
  // Animation du bouton au survol
  Color buttonColor = isHovering ? 
    Color{70, 40, 40, 255} : Color{50, 30, 30, 255};
  Color buttonBorder = isHovering ? 
    Color{230, 80, 80, 255} : Color{180, 60, 60, 255};
  
  // Effet de surbrillance sur le bouton au survol
  if (isHovering) {
    Rectangle glowButton = {
      reconnectButton.x - 2, reconnectButton.y - 2,
      reconnectButton.width + 4, reconnectButton.height + 4
    };
    float glowAlpha = (sinf(time * 5.0f) + 1.0f) * 0.5f * 180 + 75;
    CustomRayLib::drawRectangleRec(glowButton, Color{200, 50, 50, (unsigned char)glowAlpha});
  }
  
  CustomRayLib::drawRectangleRec(reconnectButton, buttonColor);
  CustomRayLib::drawRectangleLines(reconnectButton.x, reconnectButton.y, 
                                 reconnectButton.width, reconnectButton.height, 
                                 buttonBorder);
  
  const char* buttonText = "RETURN TO MENU";
  int buttonTextWidth = CustomRayLib::measureText(buttonText, 16);
  
  // Texte du bouton avec effet de brillance
  CustomRayLib::drawText(buttonText, 
                       reconnectButton.x + (reconnectButton.width - buttonTextWidth) / 2 + 1, 
                       reconnectButton.y + 15 + 1, 
                       16, Color{0, 0, 0, 100});
  CustomRayLib::drawText(buttonText, 
                       reconnectButton.x + (reconnectButton.width - buttonTextWidth) / 2, 
                       reconnectButton.y + 15, 
                       16, Color{255, 255, 255, 255});
  
  // Gestion du clic sur le bouton
  if (CustomRayLib::isMouseButtonPressed(MOUSE_LEFT_BUTTON) && isHovering) {
    // Réinitialiser l'état de l'application
    setServerStatus(0);
    setGuiIsListening(false);
    networkClient.reset();
    packetParser.reset();
  }
  
  // Afficher le temps de déconnexion
  float disconnectTime = (float)CustomRayLib::getTime() - disconnectTimestamp;
  char timeStr[32];
  snprintf(timeStr, sizeof(timeStr), "Disconnected %.1f seconds ago", disconnectTime);
  
  int timeStrWidth = CustomRayLib::measureText(timeStr, 12);
  CustomRayLib::drawText(timeStr, 
                       centerX - timeStrWidth / 2, 
                       mainPanel.y + mainPanel.height - 25, 
                       12, Color{150, 150, 150, 255});
}*/

/*oid Game::drawEndOfTheGameUI() {
  // Fond avec effet de particules de célébration
  float time = (float)CustomRayLib::getTime() * 0.5f;
  Color bgColor1 = {30, 40, 60, 255}; // Bleu foncé
  Color bgColor2 = {20, 30, 50, 255}; // Bleu encore plus foncé
  
  // Dégradé de fond animé
  for (int i = 0; i < windowHeight; i += 3) {
    float factor = (sinf(time * 0.2f + i * 0.005f) + 1.0f) * 0.5f;
    Color color = {
      (unsigned char)(bgColor1.r * factor + bgColor2.r * (1 - factor)),
      (unsigned char)(bgColor1.g * factor + bgColor2.g * (1 - factor)),
      (unsigned char)(bgColor1.b * factor + bgColor2.b * (1 - factor)),
      255
    };
    CustomRayLib::drawLine(0, i, windowWidth, i, color);
  }
  
  // Particules de célébration (confettis)
  for (int i = 0; i < 100; i++) {
    float size = (sinf(time * 2.0f + i * 3.7f) + 1.0f) * 2.0f + 1.0f;
    float x = (sinf(time + i * 2.3f) + 1.0f) * windowWidth * 0.5f;
    float y = (time * 30.0f + i * 50.0f) - (int)((time * 30.0f + i * 50.0f) / windowHeight) * windowHeight;
    
    // Couleurs vives et variées pour les confettis
    Color particleColor;
    int colorType = (i + (int)(time * 3)) % 5;
    switch (colorType) {
      case 0: particleColor = {255, 100, 100, 200}; break; // Rouge
      case 1: particleColor = {100, 255, 100, 200}; break; // Vert
      case 2: particleColor = {100, 100, 255, 200}; break; // Bleu
      case 3: particleColor = {255, 255, 100, 200}; break; // Jaune
      case 4: particleColor = {255, 100, 255, 200}; break; // Rose
    }
    
    CustomRayLib::drawRectangle(x, y, size * 2, size * 2, particleColor);
  }
  
  // Calculer les positions au centre de l'écran
  int centerX = windowWidth / 2;
  int centerY = windowHeight / 2;
  
  // Effet de pulsation pour le panneau
  float pulse = sinf(time * 1.5f) * 0.03f + 1.0f;
  float panelWidth = 500 * pulse;
  float panelHeight = 350 * pulse;
  
  // Panel principal avec ombre
  Rectangle mainPanel = {
    static_cast<float>(centerX - panelWidth / 2),
    static_cast<float>(centerY - panelHeight / 2),
    panelWidth,
    panelHeight
  };
  
  // Ombre du panel
  Rectangle shadowPanel = {
    mainPanel.x + 10,
    mainPanel.y + 10,
    mainPanel.width,
    mainPanel.height
  };
  CustomRayLib::drawRectangleRec(shadowPanel, Color{0, 0, 0, 30});
  
  // Panel principal avec effet de dégradé
  CustomRayLib::drawRectangleRec(mainPanel, Color{35, 45, 70, 230});
  
  // Bordure dorée animée
  Color goldColor1 = {255, 215, 0, 255}; // Or brillant
  Color goldColor2 = {218, 165, 32, 255}; // Or plus foncé
  
  for (int i = 0; i < 4; i++) {
    float alpha = (sinf(time * 3.0f + i * 0.8f) + 1.0f) * 0.5f;
    Color borderColor = {
      (unsigned char)(goldColor1.r * alpha + goldColor2.r * (1 - alpha)),
      (unsigned char)(goldColor1.g * alpha + goldColor2.g * (1 - alpha)),
      (unsigned char)(goldColor1.b * alpha + goldColor2.b * (1 - alpha)),
      255
    };
    CustomRayLib::drawRectangleLines(
      mainPanel.x - i, mainPanel.y - i, 
      mainPanel.width + i * 2, mainPanel.height + i * 2, 
      borderColor
    );
  }
  
  // Dessin d'une coupe/trophée stylisée
  float trophyBaseX = centerX;
  float trophyBaseY = centerY - 80;
  float trophySize = 60.0f + sinf(time * 2.0f) * 3.0f; // Taille avec légère pulsation
  
  // Base du trophée
  CustomRayLib::drawRectangle(trophyBaseX - 30, trophyBaseY + 60, 60, 10, Color{218, 165, 32, 255});
  CustomRayLib::drawRectangle(trophyBaseX - 20, trophyBaseY + 50, 40, 10, Color{255, 215, 0, 255});
  
  // Pied du trophée
  CustomRayLib::drawRectangle(trophyBaseX - 5, trophyBaseY + 20, 10, 30, Color{255, 215, 0, 255});
  
  // Coupe
  CustomRayLib::drawCircleSector(Vector2{trophyBaseX, trophyBaseY}, trophySize * 0.4f, 
                              180, 360, 20, Color{255, 215, 0, 255});
  CustomRayLib::drawCircleSector(Vector2{trophyBaseX, trophyBaseY}, trophySize * 0.35f, 
                              180, 360, 20, Color{255, 235, 50, 255});
  
  // Anses
  CustomRayLib::drawCircleLines(trophyBaseX - trophySize * 0.35f, trophyBaseY, trophySize * 0.15f, Color{255, 215, 0, 255});
  CustomRayLib::drawCircleLines(trophyBaseX + trophySize * 0.35f, trophyBaseY, trophySize * 0.15f, Color{255, 215, 0, 255});
  
  // Étincelles autour du trophée
  for (int i = 0; i < 20; i++) {
    float angle = time * 2.0f + i * 18.0f;
    float radius = trophySize * 0.7f + sinf(time * 3.0f + i * 0.5f) * 10.0f;
    float sparkX = trophyBaseX + cosf(angle) * radius;
    float sparkY = trophyBaseY + sinf(angle) * radius;
    float sparkSize = (sinf(time * 4.0f + i * 0.7f) + 1.0f) * 2.0f + 1.0f;
    
    Color sparkColor = {
      255,
      (unsigned char)(215 + sinf(time + i) * 40),
      (unsigned char)(sinf(time * 2.0f + i) * 50 + 50),
      (unsigned char)(200 + sinf(time * 3.0f + i) * 55)
    };
    
    CustomRayLib::drawRectangle(sparkX, sparkY, sparkSize, sparkSize, sparkColor);
  }
  
  // Titre principal avec effet de lueur
  const char* titlePart1 = "GAME";
  const char* titlePart2 = "OVER";
  float titleFontSize = 36;
  int titlePart1Width = CustomRayLib::measureText(titlePart1, titleFontSize);
  int titlePart2Width = CustomRayLib::measureText(titlePart2, titleFontSize);
  float spacing = 20.0f;
  
  // Position calculée pour centrer "GAME OVER"
  float titleX1 = centerX - (titlePart1Width + titlePart2Width + spacing) / 2;
  float titleX2 = titleX1 + titlePart1Width + spacing;
  
  // Effet de couleur animé pour le titre
  Color titleColor1 = {
    (unsigned char)(200 + sinf(time * 3.0f) * 55),
    (unsigned char)(180 + sinf(time * 4.0f) * 75),
    0,
    255
  };
  
  Color titleColor2 = {
    (unsigned char)(220 + sinf(time * 2.5f) * 35),
    (unsigned char)(50 + sinf(time * 3.5f) * 25),
    (unsigned char)(50 + sinf(time * 4.5f) * 25),
    255
  };
  
  // Ombre du titre
  CustomRayLib::drawText(titlePart1, titleX1 + 3, centerY - 120 + 3, titleFontSize, Color{0, 0, 0, 100});
  CustomRayLib::drawText(titlePart2, titleX2 + 3, centerY - 120 + 3, titleFontSize, Color{0, 0, 0, 100});
  
  // Texte du titre
  CustomRayLib::drawText(titlePart1, titleX1, centerY - 120, titleFontSize, titleColor1);
  CustomRayLib::drawText(titlePart2, titleX2, centerY - 120, titleFontSize, titleColor2);
  
  // Récupérer le nom de l'équipe gagnante
  std::string winnerTeam = winningTeam; // À remplacer par votre logique pour récupérer l'équipe gagnante

  // Annonce du vainqueur
  const char* winnerPrefix = "WINNER:";
  int prefixWidth = CustomRayLib::measureText(winnerPrefix, 20);
  CustomRayLib::drawText(winnerPrefix, centerX - prefixWidth / 2, centerY + 10, 20, Color{255, 255, 255, 200});
  
  // Nom de l'équipe gagnante en grand et brillant
  int teamNameWidth = CustomRayLib::measureText(winnerTeam.c_str(), 32);
  
  // Effet de pulsation pour le nom de l'équipe
  float teamNameScale = 1.0f + sinf(time * 2.0f) * 0.05f;
  float scaledFontSize = 32 * teamNameScale;
  
  // Recalculer la largeur avec la nouvelle taille
  teamNameWidth = CustomRayLib::measureText(winnerTeam.c_str(), scaledFontSize);
  
  // Ombre du texte
  CustomRayLib::drawText(winnerTeam.c_str(), centerX - teamNameWidth / 2 + 2, centerY + 40 + 2, scaledFontSize, Color{0, 0, 0, 100});
  
  // Couleur animée pour le nom de l'équipe
  Color teamNameColor = {
    (unsigned char)(200 + sinf(time * 2.0f) * 55),
    (unsigned char)(200 + sinf(time * 3.0f + 1.0f) * 55),
    (unsigned char)(50 + sinf(time * 4.0f + 2.0f) * 50),
    255
  };
  
  CustomRayLib::drawText(winnerTeam.c_str(), centerX - teamNameWidth / 2, centerY + 40, scaledFontSize, teamNameColor);
  
  // Ligne de séparation
  CustomRayLib::drawLine(centerX - 100, centerY + 115, centerX + 100, centerY + 115, Color{255, 215, 0, 150});
  
  // Bouton pour retourner au menu principal
  Rectangle menuButton = {
    static_cast<float>(centerX - 120),
    static_cast<float>(centerY + 170),
    240,
    45
  };
  
  Vector2 mousePos = CustomRayLib::getMousePosition();
  bool isHovering = CustomRayLib::checkCollisionPointRec(mousePos, menuButton);
  
  // Animation du bouton au survol
  Color buttonColor = isHovering ? 
    Color{60, 70, 120, 255} : Color{40, 50, 90, 255};
  Color buttonBorder = isHovering ? 
    Color{255, 215, 0, 255} : Color{218, 165, 32, 255};
  
  // Effet de surbrillance sur le bouton au survol
  if (isHovering) {
    Rectangle glowButton = {
      menuButton.x - 3, menuButton.y - 3,
      menuButton.width + 6, menuButton.height + 6
    };
    float glowAlpha = (sinf(time * 5.0f) + 1.0f) * 0.5f * 180 + 75;
    CustomRayLib::drawRectangleRec(glowButton, Color{255, 215, 0, (unsigned char)(glowAlpha * 0.5f)});
  }
  
  CustomRayLib::drawRectangleRec(menuButton, buttonColor);
  CustomRayLib::drawRectangleLines(menuButton.x, menuButton.y, 
                                 menuButton.width, menuButton.height, 
                                 buttonBorder);
  
  const char* buttonText = "RETURN TO MAIN MENU";
  int buttonTextWidth = CustomRayLib::measureText(buttonText, 16);
  
  // Texte du bouton avec effet de brillance
  CustomRayLib::drawText(buttonText, 
                       menuButton.x + (menuButton.width - buttonTextWidth) / 2 + 1, 
                       menuButton.y + 15 + 1, 
                       16, Color{0, 0, 0, 100});
  CustomRayLib::drawText(buttonText, 
                       menuButton.x + (menuButton.width - buttonTextWidth) / 2, 
                       menuButton.y + 15, 
                       16, Color{255, 255, 255, 255});
  
  // Gestion du clic sur le bouton
  if (CustomRayLib::isMouseButtonPressed(MOUSE_LEFT_BUTTON) && isHovering) {
    // Réinitialiser l'application
    setServerStatus(0);
    setGuiIsListening(false);
    networkClient.reset();
    packetParser.reset();
    teams.clear();
    tiles.clear();
  }
}*/
