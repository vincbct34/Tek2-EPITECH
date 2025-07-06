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

void Game::drawEndOfTheGameUI() {
  float time = (float)CustomRayLib::getTime() * 0.5f;
  int centerX = windowWidth / 2;
  int centerY = windowHeight / 2;
  
  drawEndGameBackground(time);
  drawEndGameConfetti(time);
  drawEndGamePanel(time, centerX, centerY);
  drawEndGameTrophy(time, centerX, centerY);
  drawEndGameTitle(time, centerX, centerY);
  drawEndGameWinner(time, centerX, centerY);
  drawEndGameButton(time, centerX, centerY);
}

void Game::drawEndGameBackground(float time) {
  Color bgColor1 = {30, 40, 60, 255};
  Color bgColor2 = {20, 30, 50, 255};

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
}

void Game::drawEndGameConfetti(float time) {
  for (int i = 0; i < 100; i++) {
    float size = (sinf(time * 2.0f + i * 3.7f) + 1.0f) * 2.0f + 1.0f;
    float x = (sinf(time + i * 2.3f) + 1.0f) * windowWidth * 0.5f;
    float y = (time * 30.0f + i * 50.0f) - (int)((time * 30.0f + i * 50.0f) / windowHeight) * windowHeight;
    Color particleColor = getConfettiColor(i, time);

    CustomRayLib::drawRectangle(x, y, size * 2, size * 2, particleColor);
  }
}

Color Game::getConfettiColor(int index, float time) {
  int colorType = (index + (int)(time * 3)) % 5;

  switch (colorType) {
    case 0: return {255, 100, 100, 200};
    case 1: return {100, 255, 100, 200};
    case 2: return {100, 100, 255, 200};
    case 3: return {255, 255, 100, 200};
    case 4: return {255, 100, 255, 200};
    default: return {255, 255, 255, 200};
  }
}

void Game::drawEndGamePanel(float time, int centerX, int centerY) {
  float pulse = sinf(time * 1.5f) * 0.03f + 1.0f;
  float panelWidth = 500 * pulse;
  float panelHeight = 350 * pulse;
  Rectangle mainPanel = {
    static_cast<float>(centerX - panelWidth / 2),
    static_cast<float>(centerY - panelHeight / 2),
    panelWidth,
    panelHeight
  };
  Rectangle shadowPanel = {
    mainPanel.x + 10,
    mainPanel.y + 10,
    mainPanel.width,
    mainPanel.height
  };

  CustomRayLib::drawRectangleRec(shadowPanel, Color{0, 0, 0, 30});
  CustomRayLib::drawRectangleRec(mainPanel, Color{35, 45, 70, 230});
  drawEndGameGoldenBorder(time, mainPanel);
}

void Game::drawEndGameGoldenBorder(float time, const Rectangle& mainPanel) {
  Color goldColor1 = {255, 215, 0, 255};
  Color goldColor2 = {218, 165, 32, 255};

  for (int i = 0; i < 4; i++) {
    float alpha = (sinf(time * 3.0f + i * 0.8f) + 1.0f) * 0.5f;
    Color borderColor = {
      (unsigned char)(goldColor1.r * alpha + goldColor2.r * (1 - alpha)),
      (unsigned char)(goldColor1.g * alpha + goldColor2.g * (1 - alpha)),
      (unsigned char)(goldColor1.b * alpha + goldColor2.b * (1 - alpha)),
      255
    };

    CustomRayLib::drawRectangleLines(mainPanel.x - i, mainPanel.y - i, mainPanel.width + i * 2, mainPanel.height + i * 2, borderColor);
  }
}

void Game::drawEndGameTrophy(float time, int centerX, int centerY) {
  float trophyBaseX = centerX;
  float trophyBaseY = centerY - 80;
  float trophySize = 60.0f + sinf(time * 2.0f) * 3.0f;

  drawTrophyBase(trophyBaseX, trophyBaseY);
  drawTrophyCup(trophyBaseX, trophyBaseY, trophySize);
  drawTrophyHandles(trophyBaseX, trophyBaseY, trophySize);
  drawTrophySparkles(time, trophyBaseX, trophyBaseY, trophySize);
}

void Game::drawTrophyBase(float trophyBaseX, float trophyBaseY) {
  CustomRayLib::drawRectangle(trophyBaseX - 30, trophyBaseY + 60, 60, 10, Color{218, 165, 32, 255});
  CustomRayLib::drawRectangle(trophyBaseX - 20, trophyBaseY + 50, 40, 10, Color{255, 215, 0, 255});
  CustomRayLib::drawRectangle(trophyBaseX - 5, trophyBaseY + 20, 10, 30, Color{255, 215, 0, 255});
}

void Game::drawTrophyCup(float trophyBaseX, float trophyBaseY, float trophySize) {
  CustomRayLib::drawCircleSector(Vector2{trophyBaseX, trophyBaseY}, trophySize * 0.4f, 180, 360, 20, Color{255, 215, 0, 255});
  CustomRayLib::drawCircleSector(Vector2{trophyBaseX, trophyBaseY}, trophySize * 0.35f, 180, 360, 20, Color{255, 235, 50, 255});
}

void Game::drawTrophyHandles(float trophyBaseX, float trophyBaseY, float trophySize) {
  CustomRayLib::drawCircleLines(trophyBaseX - trophySize * 0.35f, trophyBaseY, trophySize * 0.15f, Color{255, 215, 0, 255});
  CustomRayLib::drawCircleLines(trophyBaseX + trophySize * 0.35f, trophyBaseY, trophySize * 0.15f, Color{255, 215, 0, 255});
}

void Game::drawTrophySparkles(float time, float trophyBaseX, float trophyBaseY, float trophySize) {
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
}

void Game::drawEndGameTitle(float time, int centerX, int centerY) {
  const char* titlePart1 = "GAME";
  const char* titlePart2 = "OVER";
  float titleFontSize = 36;
  int titlePart1Width = CustomRayLib::measureText(titlePart1, titleFontSize);
  int titlePart2Width = CustomRayLib::measureText(titlePart2, titleFontSize);
  float spacing = 20.0f;
  float titleX1 = centerX - (titlePart1Width + titlePart2Width + spacing) / 2;
  float titleX2 = titleX1 + titlePart1Width + spacing;
  Color titleColor1 = getTitleColor1(time);
  Color titleColor2 = getTitleColor2(time);
  
  CustomRayLib::drawText(titlePart1, titleX1 + 3, centerY - 120 + 3, titleFontSize, Color{0, 0, 0, 100});
  CustomRayLib::drawText(titlePart2, titleX2 + 3, centerY - 120 + 3, titleFontSize, Color{0, 0, 0, 100});
  CustomRayLib::drawText(titlePart1, titleX1, centerY - 120, titleFontSize, titleColor1);
  CustomRayLib::drawText(titlePart2, titleX2, centerY - 120, titleFontSize, titleColor2);
}

Color Game::getTitleColor1(float time) {
  return {
    (unsigned char)(200 + sinf(time * 3.0f) * 55),
    (unsigned char)(180 + sinf(time * 4.0f) * 75),
    0,
    255
  };
}

Color Game::getTitleColor2(float time) {
  return {
    (unsigned char)(220 + sinf(time * 2.5f) * 35),
    (unsigned char)(50 + sinf(time * 3.5f) * 25),
    (unsigned char)(50 + sinf(time * 4.5f) * 25),
    255
  };
}

void Game::drawEndGameWinner(float time, int centerX, int centerY) {
  std::string winnerTeam = winningTeam;
  const char* winnerPrefix = "WINNER:";
  int prefixWidth = CustomRayLib::measureText(winnerPrefix, 20);

  CustomRayLib::drawText(winnerPrefix, centerX - prefixWidth / 2, centerY + 10, 20, Color{255, 255, 255, 200});
  drawWinnerTeamName(time, centerX, centerY, winnerTeam);
  CustomRayLib::drawLine(centerX - 100, centerY + 115, centerX + 100, centerY + 115, Color{255, 215, 0, 150});
}

void Game::drawWinnerTeamName(float time, int centerX, int centerY, const std::string& winnerTeam) {
  int teamNameWidth = CustomRayLib::measureText(winnerTeam.c_str(), 32);
  float teamNameScale = 1.0f + sinf(time * 2.0f) * 0.05f;
  float scaledFontSize = 32 * teamNameScale;

  teamNameWidth = CustomRayLib::measureText(winnerTeam.c_str(), scaledFontSize);
  
  CustomRayLib::drawText(winnerTeam.c_str(), centerX - teamNameWidth / 2 + 2, centerY + 40 + 2, scaledFontSize, Color{0, 0, 0, 100});
  Color teamNameColor = {
    (unsigned char)(200 + sinf(time * 2.0f) * 55),
    (unsigned char)(200 + sinf(time * 3.0f + 1.0f) * 55),
    (unsigned char)(50 + sinf(time * 4.0f + 2.0f) * 50),
    255
  };
  CustomRayLib::drawText(winnerTeam.c_str(), centerX - teamNameWidth / 2, centerY + 40, scaledFontSize, teamNameColor);
}

void Game::drawEndGameButton(float time, int centerX, int centerY) {
  Rectangle menuButton = {
    static_cast<float>(centerX - 120),
    static_cast<float>(centerY + 170),
    240,
    45
  };
  Vector2 mousePos = CustomRayLib::getMousePosition();
  bool isHovering = CustomRayLib::checkCollisionPointRec(mousePos, menuButton);
  
  Color buttonColor = isHovering ? 
    Color{60, 70, 120, 255} : Color{40, 50, 90, 255};
  Color buttonBorder = isHovering ? 
    Color{255, 215, 0, 255} : Color{218, 165, 32, 255};
  
  if (isHovering)
    drawEndGameButtonGlow(time, menuButton);
  CustomRayLib::drawRectangleRec(menuButton, buttonColor);
  CustomRayLib::drawRectangleLines(menuButton.x, menuButton.y, menuButton.width, menuButton.height, buttonBorder);
  drawEndGameButtonText(menuButton);
  handleEndGameButtonClick(isHovering);
}

void Game::drawEndGameButtonGlow(float time, const Rectangle& menuButton) {
  Rectangle glowButton = {
    menuButton.x - 3, menuButton.y - 3,
    menuButton.width + 6, menuButton.height + 6
  };
  float glowAlpha = (sinf(time * 5.0f) + 1.0f) * 0.5f * 180 + 75;

  CustomRayLib::drawRectangleRec(glowButton, Color{255, 215, 0, (unsigned char)(glowAlpha * 0.5f)});
}

void Game::drawEndGameButtonText(const Rectangle& menuButton) {
  const char* buttonText = "RETURN TO MAIN MENU";
  int buttonTextWidth = CustomRayLib::measureText(buttonText, 16);
  
  CustomRayLib::drawText(buttonText, 
                       menuButton.x + (menuButton.width - buttonTextWidth) / 2 + 1, 
                       menuButton.y + 15 + 1, 
                       16, Color{0, 0, 0, 100});
  CustomRayLib::drawText(buttonText, 
                       menuButton.x + (menuButton.width - buttonTextWidth) / 2, 
                       menuButton.y + 15, 
                       16, Color{255, 255, 255, 255});
}

void Game::handleEndGameButtonClick(bool isHovering) {
  if (CustomRayLib::isMouseButtonPressed(MOUSE_LEFT_BUTTON) && isHovering) {
    setServerStatus(0);
    setGuiIsListening(false);
    networkClient.reset();
    packetParser.reset();
    teams.clear();
    tiles.clear();
  }
}
