/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** AssetManager
*/

#pragma once

#include "Exceptions.hpp"

#include <SFML/Graphics.hpp>
#include <unordered_map>
#include <string>

class AssetManager {
public:

    /**
     * @brief Constructs an AssetManager object.
     */
    AssetManager();

    /**
     * @brief Destroys the AssetManager object.
     */
    ~AssetManager() = default;

    /**
     * @brief Retrieves a texture by name.
     *
     * @param name The name of the texture.
     * @return A reference to the texture.
     */
    sf::Texture& getTexture(const std::string& name);
    /**
     * @brief Loads a texture from a file.
     *
     * @param name The name of the texture.
     * @param path The file path to the texture.
     */
    void loadTexture(const std::string& name, const std::string& path);
    /**
     * @brief Retrieves a sprite by name.
     *
     * @param name The name of the sprite.
     * @return A reference to the sprite.
     */
    sf::Sprite& getSprite(const std::string& name);
    /**
     * @brief Loads a sprite from a texture.
     *
     * @param name The name of the sprite.
     * @param textureName The name of the texture to use for the sprite.
     */
    void loadSprite(const std::string& name);
    /**
     * @brief Loads all textures from a directory.
     */
    void loadAllTextures();
    /**
     * @brief Loads all sprites from a directory.
     */
    void loadAllSprites();

private:
    std::unordered_map<std::string, sf::Texture> _textures; // Map of texture names to textures
    std::unordered_map<std::string, sf::Sprite> _sprites; // Map of sprite names to sprites
};
