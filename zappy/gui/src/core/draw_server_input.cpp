/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** draw_server_input
*/

#include "Game.hpp"
#include "network/NetworkClient.hpp"
#include "network/PacketParser.hpp"
#include <cmath>

void Game::drawServerInputUI() {
  CustomRayLib::clearBackground(Color{30, 30, 40, 255});
  int centerX = windowWidth / 2;
  int centerY = windowHeight / 2;
  
  drawMainPanel(centerX, centerY);
  drawTitle(centerX, centerY);
  drawInputBoxes(centerX, centerY);
  drawConnectButton(centerX, centerY);
  drawInstructions(centerX, centerY);
  handleConnectionAttempt(centerX, centerY);
}

void Game::drawMainPanel(int centerX, int centerY) {
  int boxWidth = 350;
  
  Rectangle mainPanel = {
    static_cast<float>(centerX - boxWidth / 2 - 60),
    static_cast<float>(centerY - 200),
    static_cast<float>(boxWidth + 120),
    static_cast<float>(420)
  };
  Rectangle shadowPanel = {
    mainPanel.x + 5,
    mainPanel.y + 5,
    mainPanel.width,
    mainPanel.height
  };
  CustomRayLib::drawRectangleRec(shadowPanel, Color{0, 0, 0, 50});
  CustomRayLib::drawRectangleRec(mainPanel, Color{50, 50, 60, 255});
  CustomRayLib::drawRectangleLines(mainPanel.x, mainPanel.y, mainPanel.width, mainPanel.height, Color{80, 80, 90, 255});
}

void Game::drawTitle(int centerX, int centerY) {
  const char* title = "ZAPPY";
  const char* subtitle = "Server Connection";
  int titleWidth = CustomRayLib::measureText(title, 36);
  int subtitleWidth = CustomRayLib::measureText(subtitle, 18);
  
  CustomRayLib::drawText(title, centerX - titleWidth / 2 + 2, centerY - 130 + 2, 36, Color{0, 0, 0, 100}); // Ombre
  CustomRayLib::drawText(title, centerX - titleWidth / 2, centerY - 130, 36, Color{100, 200, 255, 255}); // Bleu clair
  CustomRayLib::drawText(subtitle, centerX - subtitleWidth / 2, centerY - 95, 18, Color{200, 200, 200, 255});
  CustomRayLib::drawLine(centerX - 100, centerY - 75, centerX + 100, centerY - 75, Color{80, 80, 90, 255});
}

void Game::drawInputBoxes(int centerX, int centerY) {
  int boxWidth = 350;
  int boxHeight = 55;
  int spacing = 25;
  
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
  handleInputBoxClicks(ipBox, portBox);
  handleTextInput();
  drawIPBox(ipBox);
  drawPortBox(portBox);
  drawInputLabels(ipBox, portBox);
  drawInputContent(ipBox, portBox);
}

void Game::handleInputBoxClicks(const Rectangle& ipBox, const Rectangle& portBox) {
  Vector2 mousePos = CustomRayLib::getMousePosition();

  if (CustomRayLib::isMouseButtonPressed(MOUSE_LEFT_BUTTON)) {
    ipBoxActive = CustomRayLib::checkCollisionPointRec(mousePos, ipBox);
    portBoxActive = CustomRayLib::checkCollisionPointRec(mousePos, portBox);
  }
}

void Game::drawIPBox(const Rectangle& ipBox) {
  Color ipBoxColor = ipBoxActive ? Color{70, 130, 180, 255} : Color{60, 60, 70, 255};
  Color ipBorderColor = ipBoxActive ? Color{100, 200, 255, 255} : Color{100, 100, 120, 255};
  CustomRayLib::drawRectangleRec(ipBox, ipBoxColor);
  CustomRayLib::drawRectangleLines(ipBox.x, ipBox.y, ipBox.width, ipBox.height, ipBorderColor);

  if (ipBoxActive)
    CustomRayLib::drawRectangleLines(ipBox.x - 1, ipBox.y - 1, ipBox.width + 2, ipBox.height + 2, Color{100, 200, 255, 100});
}

void Game::drawPortBox(const Rectangle& portBox) {
  Color portBoxColor = portBoxActive ? Color{70, 130, 180, 255} : Color{60, 60, 70, 255};
  Color portBorderColor = portBoxActive ? Color{100, 200, 255, 255} : Color{100, 100, 120, 255};
  CustomRayLib::drawRectangleRec(portBox, portBoxColor);
  CustomRayLib::drawRectangleLines(portBox.x, portBox.y, portBox.width, portBox.height, portBorderColor);
  
  if (portBoxActive)
    CustomRayLib::drawRectangleLines(portBox.x - 1, portBox.y - 1, portBox.width + 2, portBox.height + 2, Color{100, 200, 255, 100});
}

void Game::drawInputLabels(const Rectangle& ipBox, const Rectangle& portBox) {
  CustomRayLib::drawText("IP Address", ipBox.x, ipBox.y - 25, 16, Color{200, 200, 200, 255});
  CustomRayLib::drawText("Port", portBox.x, portBox.y - 25, 16, Color{200, 200, 200, 255});
}

void Game::drawInputContent(const Rectangle& ipBox, const Rectangle& portBox) {
  std::string ipDisplay = ipInput.empty() ? "127.0.0.1" : ipInput;
  std::string portDisplay = portInput.empty() ? "4242" : portInput;
  Color ipTextColor = ipInput.empty() ? Color{150, 150, 150, 255} : Color{255, 255, 255, 255};
  Color portTextColor = portInput.empty() ? Color{150, 150, 150, 255} : Color{255, 255, 255, 255};
  
  if (ipBoxActive && ((int)(CustomRayLib::getTime() * 2) % 2)) {
    ipDisplay += "|";
    ipTextColor = Color{255, 255, 255, 255};
  }
  if (portBoxActive && ((int)(CustomRayLib::getTime() * 2) % 2)) {
    portDisplay += "|";
    portTextColor = Color{255, 255, 255, 255};
  }
  CustomRayLib::drawText(ipDisplay.c_str(), ipBox.x + 15, ipBox.y + 18, 18, ipTextColor);
  CustomRayLib::drawText(portDisplay.c_str(), portBox.x + 15, portBox.y + 18, 18, portTextColor);
}

void Game::drawConnectButton(int centerX, int centerY) {
  Rectangle connectButton = {
    static_cast<float>(centerX - 80),
    static_cast<float>(centerY + 100),
    160,
    45
  };
  Vector2 mousePos = CustomRayLib::getMousePosition();
  bool isHovering = CustomRayLib::checkCollisionPointRec(mousePos, connectButton);
  Color buttonColor = isHovering ? Color{70, 130, 180, 255} : Color{50, 100, 150, 255};
  Color buttonBorder = isHovering ? Color{100, 200, 255, 255} : Color{80, 150, 200, 255};
  
  CustomRayLib::drawRectangleRec(connectButton, buttonColor);
  CustomRayLib::drawRectangleLines(connectButton.x, connectButton.y, connectButton.width, connectButton.height, buttonBorder);
  
  const char* buttonText = "CONNECT";
  int buttonTextWidth = CustomRayLib::measureText(buttonText, 16);

  CustomRayLib::drawText(buttonText, connectButton.x + (connectButton.width - buttonTextWidth) / 2, connectButton.y + 14, 16, Color{255, 255, 255, 255});
}

void Game::drawInstructions(int centerX, int centerY) {
  const char* instructions = "Press ENTER or click CONNECT to join";
  int instructionsWidth = CustomRayLib::measureText(instructions, 14);

  CustomRayLib::drawText(instructions, centerX - instructionsWidth / 2, centerY + 160, 14, Color{150, 150, 150, 255});

  const char* statusText = "Ready to connect";
  int statusWidth = CustomRayLib::measureText(statusText, 12);

  CustomRayLib::drawText(statusText, centerX - statusWidth / 2, centerY + 180, 12, Color{100, 200, 100, 255});
}

void Game::handleConnectionAttempt(int centerX, int centerY) {
  Rectangle connectButton = {
    static_cast<float>(centerX - 80),
    static_cast<float>(centerY + 100),
    160,
    45
  };
  Vector2 mousePos = CustomRayLib::getMousePosition();
  bool isHovering = CustomRayLib::checkCollisionPointRec(mousePos, connectButton);
  bool shouldConnect = CustomRayLib::isKeyPressed(KEY_ENTER) || (CustomRayLib::isMouseButtonPressed(MOUSE_LEFT_BUTTON) && isHovering);
  
  if (shouldConnect) {
    std::string finalIP = ipInput.empty() ? "127.0.0.1" : ipInput;
    std::string finalPort = portInput.empty() ? "4242" : portInput;
    
    if (isValidIP(finalIP) && isValidPort(finalPort)) {
      setServerIp(finalIP);
      setServerPort(finalPort);
      tiles.clear();
      teams.clear();
      setServerStatus(1);
      std::cout << "[DEBUG] Server info set: " << finalIP << ":" << finalPort << std::endl;
    } else {
      std::cout << "[ERROR] Invalid IP or Port format!" << std::endl;
    }
  }
}
