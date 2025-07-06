/*
** EPITECH PROJECT, 2025
** zappy_remote
** File description:
** draw_disconnected_screen
*/

#include "Game.hpp"
#include "network/NetworkClient.hpp"
#include "network/PacketParser.hpp"
#include <cmath>

void Game::drawDisconnectedUI() {
  float time = (float)CustomRayLib::getTime() * 0.2f;
  int centerX = windowWidth / 2;
  int centerY = windowHeight / 2;
  
  drawDisconnectedBackground(time);
  drawDisconnectedPanel(time, centerX, centerY);
  drawDisconnectedIcon(centerX, centerY);
  drawDisconnectedTitle(time, centerX, centerY);
  drawDisconnectedMessages(centerX, centerY);
  drawDisconnectedButton(time, centerX, centerY);
  drawDisconnectionTime(centerX, centerY);
}

void Game::drawDisconnectedBackground(float time) {
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
}

void Game::drawDisconnectedPanel(float time, int centerX, int centerY) {
  float pulse = sinf(time * 3.0f) * 0.05f + 1.0f;
  float panelWidth = 450 * pulse;
  float panelHeight = 280 * pulse;
  
  Rectangle mainPanel = {
    static_cast<float>(centerX - panelWidth / 2),
    static_cast<float>(centerY - panelHeight / 2),
    panelWidth,
    panelHeight
  };
  Rectangle shadowPanel = {
    mainPanel.x + 8,
    mainPanel.y + 8,
    mainPanel.width,
    mainPanel.height
  };
  CustomRayLib::drawRectangleRec(shadowPanel, Color{0, 0, 0, 40});
  CustomRayLib::drawRectangleRec(mainPanel, Color{40, 40, 55, 255});
  drawDisconnectedBorder(time, mainPanel);
}

void Game::drawDisconnectedBorder(float time, const Rectangle& mainPanel) {
  Color borderColor1 = {180, 60, 60, 255};
  Color borderColor2 = {100, 30, 30, 255};
  
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
}

void Game::drawDisconnectedIcon(int centerX, int centerY) {
  float iconSize = 64.0f;
  float iconY = centerY - iconSize - 15;
  
  CustomRayLib::drawCircle(centerX, iconY, iconSize/2 + 5, Color{80, 10, 10, 255});
  CustomRayLib::drawCircle(centerX, iconY, iconSize/2, Color{190, 30, 30, 255});
  CustomRayLib::drawRectangle(centerX - 5, iconY - 20, 10, 30, WHITE);
  CustomRayLib::drawCircle(centerX, iconY + 20, 5, WHITE);
}

void Game::drawDisconnectedTitle(float time, int centerX, int centerY) {
  const char* title = "CONNECTION LOST";
  float titleFontSize = 28;
  int titleWidth = CustomRayLib::measureText(title, titleFontSize);

  CustomRayLib::drawText(title, centerX - titleWidth / 2 + 2, centerY + 2, titleFontSize, Color{0, 0, 0, 100});

  float colorPulse = (sinf(time * 4.0f) + 1.0f) * 0.5f;
  Color titleColor = {
    (unsigned char)(255 * colorPulse + 200 * (1 - colorPulse)),
    (unsigned char)(80 * colorPulse + 40 * (1 - colorPulse)),
    (unsigned char)(80 * colorPulse + 40 * (1 - colorPulse)),
    255
  };

  CustomRayLib::drawText(title, centerX - titleWidth / 2, centerY, titleFontSize, titleColor);
}

void Game::drawDisconnectedMessages(int centerX, int centerY) {
  const char* message = "The server connection has been interrupted";
  int messageWidth = CustomRayLib::measureText(message, 18);

  CustomRayLib::drawText(message, centerX - messageWidth / 2, centerY + 40, 18, Color{220, 220, 220, 255});

  const char* submessage = "Please check your network and try reconnecting";
  int submessageWidth = CustomRayLib::measureText(submessage, 16);

  CustomRayLib::drawText(submessage, centerX - submessageWidth / 2, centerY + 70, 16, Color{180, 180, 180, 255});
}

void Game::drawDisconnectedButton(float time, int centerX, int centerY) {
  Rectangle reconnectButton = {
    static_cast<float>(centerX - 100),
    static_cast<float>(centerY + 110),
    200,
    45
  };
  Vector2 mousePos = CustomRayLib::getMousePosition();
  bool isHovering = CustomRayLib::checkCollisionPointRec(mousePos, reconnectButton);
  Color buttonColor = isHovering ? 
    Color{70, 40, 40, 255} : Color{50, 30, 30, 255};
  Color buttonBorder = isHovering ? 
    Color{230, 80, 80, 255} : Color{180, 60, 60, 255};
  
  if (isHovering)
    drawDisconnectedButtonGlow(time, reconnectButton);
  CustomRayLib::drawRectangleRec(reconnectButton, buttonColor);
  CustomRayLib::drawRectangleLines(reconnectButton.x, reconnectButton.y, reconnectButton.width, reconnectButton.height, buttonBorder);
  drawDisconnectedButtonText(reconnectButton);
  handleDisconnectedButtonClick(isHovering);
}

void Game::drawDisconnectedButtonGlow(float time, const Rectangle& button) {
  Rectangle glowButton = {
    button.x - 2, button.y - 2,
    button.width + 4, button.height + 4
  };
  float glowAlpha = (sinf(time * 5.0f) + 1.0f) * 0.5f * 180 + 75;

  CustomRayLib::drawRectangleRec(glowButton, Color{200, 50, 50, (unsigned char)glowAlpha});
}

void Game::drawDisconnectedButtonText(const Rectangle& button) {
  const char* buttonText = "RETURN TO MENU";
  int buttonTextWidth = CustomRayLib::measureText(buttonText, 16);
  
  CustomRayLib::drawText(buttonText, button.x + (button.width - buttonTextWidth) / 2 + 1, button.y + 15 + 1, 16, Color{0, 0, 0, 100});
  CustomRayLib::drawText(buttonText, button.x + (button.width - buttonTextWidth) / 2, button.y + 15, 16, Color{255, 255, 255, 255});
}

void Game::handleDisconnectedButtonClick(bool isHovering) {
  if (CustomRayLib::isMouseButtonPressed(MOUSE_LEFT_BUTTON) && isHovering) {
    setServerStatus(0);
    setGuiIsListening(false);
    networkClient.reset();
    packetParser.reset();
  }
}

void Game::drawDisconnectionTime(int centerX, int centerY) {
  float disconnectTime = (float)CustomRayLib::getTime() - disconnectTimestamp;
  char timeStr[32];
  snprintf(timeStr, sizeof(timeStr), "Disconnected %.1f seconds ago", disconnectTime);
  int timeStrWidth = CustomRayLib::measureText(timeStr, 12);
  float panelHeight = 280;
  float panelY = centerY - panelHeight / 2;

  CustomRayLib::drawText(timeStr, 
                       centerX - timeStrWidth / 2, 
                       panelY + panelHeight - 25, 
                       12, Color{150, 150, 150, 255});
}
