/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** AssetManager
*/

#include "client/AssetManager.hpp"

AssetManager::AssetManager()
{
    loadAllTextures();
    loadAllSprites();
}

void AssetManager::loadAllTextures()
{
    loadTexture("background", "assets/background.png"); // load background texture
    loadTexture("player", "assets/player_sprite_sheet.png"); // load player texture
    loadTexture("coin", "assets/coins_sprite_sheet.png"); // load coin texture
    loadTexture("zapper", "assets/zapper_sprite_sheet.png"); // load zapper texture
}

void AssetManager::loadAllSprites()
{
    loadSprite("background"); // load background sprite
    loadSprite("player"); // load player sprite
    loadSprite("coin"); // load coin sprite
    loadSprite("zapper"); // load zapper sprite
}

sf::Texture& AssetManager::getTexture(const std::string &name)
{
    return _textures.at(name);
}

sf::Sprite& AssetManager::getSprite(const std::string &name)
{
    return _sprites.at(name);
}

void AssetManager::loadTexture(const std::string& name, const std::string& path)
{
    sf::Texture texture;

    if (!texture.loadFromFile(path))
        throw SFMLException("Failed to load texture from file: " + path);

    _textures[name] = texture; // Store the texture in the map textures
}

void AssetManager::loadSprite(const std::string& name)
{
    sf::Sprite sprite;

    if (_textures.find(name) == _textures.end())
        throw SFMLException("Texture not found: " + name);

    sprite.setTexture(_textures[name]); // Set the texture of the sprite to the loaded texture
    _sprites[name] = sprite; // Store the sprite in the map sprites
}
