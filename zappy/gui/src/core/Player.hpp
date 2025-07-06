/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** Player
*/

#pragma once

#include "core/Game.hpp"
#include "utils/CustomRayLib.hpp"
#include "core/Component.hpp"
#include "core/Egg.hpp"
#include <string>
#include <memory>

class Component;
class Egg;

class Player {
  public:
    Player(int id, int posX, int posY, int orientation, int level, int teamId);
    ~Player();

    int getPosX() const { return posX; }
    int getPosY() const { return posY; }
    int getId() const { return id; }
    int getOrientation() const { return orientation; }
    int getLevel() const { return level; }
    const std::vector<std::shared_ptr<Component>> &getInventory() const { return inventory; }
    std::vector<std::shared_ptr<Component>> &getInventory() { return inventory; }
    std::vector<std::shared_ptr<Egg>> &getEggs() { return eggs; }
    //Image getImage() const { return image; }
    //Texture2D getTexture() const { return texture; }
    Texture2D& getTexture() {
      if (!textureLoaded) {
          loadTexture();
      }
      return texture;
    }
    bool isTextureLoaded() const { return textureLoaded; }

    int getSpriteIndex() const { return spriteIndex; }
    double getStartAnimationTime() const { return startAnimationTime; }
    //void setImage(const Image &img) { image = img; }
    void setTexture(const Texture2D &tex) { texture = tex; }
    void setPosX(int x) { posX = x; }
    void setPosY(int y) { posY = y; }
    void setOrientation(int orient) { orientation = orient; }
    void setLevel(int lvl) { level = lvl; }
    void setSpriteIndex(int index) { spriteIndex = index; }
    void setStartAnimationTime(double time) { startAnimationTime = time; }
    void addEgg(std::shared_ptr<Egg> egg) { eggs.push_back(egg); }
  private:
    int id;
    int posX;
    int posY;
    int orientation; // 1: North, 2: East, 3: South, 4: West
    int level;
    std::vector<std::shared_ptr<Component>> inventory;
    std::vector<std::shared_ptr<Egg>> eggs;
    int spriteIndex = 0;
    double startAnimationTime = -1.0;
    Rectangle rectangleTeam;
    //Image image;
    int teamId; // Unique identifier for the team
    bool textureLoaded = false;
    void loadTexture();
    Texture2D texture = {};
};
