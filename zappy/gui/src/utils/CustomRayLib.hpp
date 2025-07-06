/*
** EPITECH PROJECT, 2025
** Zappy
** File description:
** CustomRayLib
*/

#pragma once

#include "raylib.h"

class CustomRayLib {
public:
  /**
   * @brief Initializes the window with the specified width, height, and title.
   *
   * @param width The width of the window.
   * @param height The height of the window.
   * @param title The title of the window.
   */
  static void initWindow(int width, int height, const char *title) {
    InitWindow(width, height, title);
  }

  /**
   * @brief Closes the window and unloads all resources.
   */
  static void closeWindow() { CloseWindow(); }

  /**
   * @brief Sets the target frames per second for the game loop.
   *
   * @param fps The desired frames per second.
   */
  static void setTargetFPS(int fps) { SetTargetFPS(fps); }

  /**
   * @brief Checks if the window should close.
   *
   * @return True if the window should close, false otherwise.
   */
  static bool windowShouldClose() { return WindowShouldClose(); }

  /**
   * @brief Begins drawing to the window.
   */
  static void beginDrawing() { BeginDrawing(); }

  /**
   * @brief Ends drawing to the window.
   */
  static void endDrawing() { EndDrawing(); }

  /**
   * @brief Clears the background with the specified color.
   *
   * @param color The color to clear the background with.
   */
  static void clearBackground(Color color) { ClearBackground(color); }

  /**
   * @brief Checks if a key is pressed.
   *
   * @param key The key to check.
   * @return True if the key is pressed, false otherwise.
   */
  static bool isKeyDown(int key) { return IsKeyDown(key); }

  /**
   * @brief Draws text on the screen.
   *
   * @param text The text to draw.
   * @param posX The x position to draw the text.
   * @param posY The y position to draw the text.
   * @param fontSize The font size of the text.
   * @param color The color of the text.
   */
  static void drawText(const char *text, int posX, int posY, int fontSize,
                       Color color) {
    DrawText(text, posX, posY, fontSize, color);
  }

  /**
   * @brief Draws a rectangle on the screen.
   *
   * @param posX The x position of the rectangle.
   * @param posY The y position of the rectangle.
   * @param width The width of the rectangle.
   * @param height The height of the rectangle.
   * @param color The color of the rectangle.
   */
  static void drawRectangle(int posX, int posY, int width, int height,
                            Color color) {
    DrawRectangle(posX, posY, width, height, color);
  }

  /**
   * @brief Draws a circle on the screen.
   *
   * @param posX The x position of the circle.
   * @param posY The y position of the circle.
   * @param radius The radius of the circle.
   * @param color The color of the circle.
   */
  static void drawCircle(int posX, int posY, float radius, Color color) {
    DrawCircle(posX, posY, radius, color);
  }

  /**
   * @brief Draws a line between two points.
   *
   * @param startX The x position of the start point.
   * @param startY The y position of the start point.
   * @param endX The x position of the end point.
   * @param endY The y position of the end point.
   * @param color The color of the line.
   */
  static void drawLine(int startX, int startY, int endX, int endY,
                       Color color) {
    DrawLine(startX, startY, endX, endY, color);
  }

  /**
   * @brief Draws a rectangle outline.
   *
   * @param posX The x position of the rectangle.
   * @param posY The y position of the rectangle.
   * @param width The width of the rectangle.
   * @param height The height of the rectangle.
   * @param color The color of the rectangle outline.
   */
  static void drawRectangleLines(int posX, int posY, int width, int height,
                                 Color color) {
    DrawRectangleLines(posX, posY, width, height, color);
  }

  /**
   * @brief Draws a rectangle with the specified Rectangle structure.
   *
   * @param rec The Rectangle structure defining the rectangle.
   * @param color The color of the rectangle.
   */
  static void drawRectangleRec(Rectangle rec, Color color) {
    DrawRectangleRec(rec, color);
  }

  /**
   * @brief Loads an image from a file.
   *
   * @param fileName The name of the file to load the image from.
   * @return The loaded Image.
   */
  static void loadImage(const char *fileName, Image &image) {
    image = LoadImage(fileName); // Load an image from a file
  }

  /**
   * @brief Resizes an image to the specified width and height.
   * 
   * @param image The image to resize.
   * @param newWidth The new width of the image.
   * @param newHeight The new height of the image.
   */
  static void imageResize(Image &image, int newWidth, int newHeight) {
    ImageResize(&image, newWidth, newHeight); // Resize an image
  }

  /**
   * @brief Loads a texture from an image.
   *
   * @param image The Image to load the texture from.
   * @return The loaded Texture2D.
   */
  static void loadTextureFromImage(const Image &image, Texture2D &texture) {
    texture = LoadTextureFromImage(image); // Load a texture from an image
  }

  /**
   * @brief Draws a texture on the screen.
   *
   * @param texture The Texture2D to draw.
   * @param posX The x position to draw the texture.
   * @param posY The y position to draw the texture.
   * @param color The color to tint the texture.
   */
  static void drawTexture(const Texture2D &texture, int posX, int posY,
                          Color color) {
    DrawTexture(texture, posX, posY, color); // Draw a texture on the screen
  }

  /**
   * @brief Draws a texture with a specified source rectangle.
   *
   * @param texture The Texture2D to draw.
   * @param source The source rectangle of the texture.
   * @param dest The destination rectangle to draw the texture.
   * @param origin The origin point for rotation.
   * @param rotation The rotation angle in degrees.
   * @param tint The color to tint the texture.
   */
  static void drawTexturePro(const Texture2D &texture, Rectangle source,
                          Rectangle dest, Vector2 origin, float rotation,
                          Color tint) {
    DrawTexturePro(texture, source, dest, origin, rotation, tint); // Draw a texture with source and destination rectangles
  }

  /**
   * @brief Unloads a texture.
   *
   * @param texture The Texture2D to unload.
   */
  static void unloadImage(Image &image) { UnloadImage(image); } // Unload an image
  /**
   * @brief Unloads a texture.
   * @param texture The Texture2D to unload.
   */
  static void unloadTexture(Texture2D &texture) { UnloadTexture(texture); } // Unload a texture

  /**
   * @brief GetTime returns the time in seconds since the program started.
   * @return The time in seconds.
   */
  static double getTime() {
    return static_cast<double>(GetTime());
  }

  /**
   * @brief GetCharPressed returns the first character pressed.
   * @return The character code of the pressed key.
   */
  static int getCharPressed() {
    return GetCharPressed();
  }

  /**
   * @brief Checks if a key is pressed.
   * @param key The key to check.
   * @return True if the key is pressed, false otherwise.
   */
  static bool isKeyPressed(int key) {
    return IsKeyPressed(key);
  }

  /**
   * @brief Gets the current mouse position.
   * @return The current mouse position as a Vector2.
   */
  static Vector2 getMousePosition() {
    return GetMousePosition();
  }

  /**
   * @brief Checks if a mouse button is pressed.
   * @param button The mouse button to check.
   * @return True if the mouse button is pressed, false otherwise.
   */
  static bool isMouseButtonPressed(int button) {
    return IsMouseButtonPressed(button);
  }

  /**
   * @brief Checks for a collision between a point and a rectangle.
   * @param point The point to check.
   * @param rec The rectangle to check.
   * @return True if there is a collision, false otherwise.
   */
  static bool checkCollisionPointRec(Vector2 point, Rectangle rec) {
    return CheckCollisionPointRec(point, rec);
  }

  /**
   * @brief Measures the width of a text string in pixels.
   * @param text The text to measure.
   * @param fontSize The font size to use for measurement.
   * @return The width of the text in pixels.
   */
  static int measureText(const char* text, int fontSize) {
    return MeasureText(text, fontSize);
  }

  /**
   * @brief Draws a circle sector.
   * @param center The center of the circle sector.
   * @param radius The radius of the circle sector.
   * @param startAngle The starting angle of the sector in degrees.
   * @param endAngle The ending angle of the sector in degrees.
   * @param segments The number of segments to use for the sector.
   * @param color The color of the sector.
   */
  static void drawCircleSector(Vector2 center, float radius, float startAngle, float endAngle, int segments, Color color) {
    DrawCircleSector(center, radius, startAngle, endAngle, segments, color);
  }

  /**
   * @brief Draws circle lines.
   * @param centerX The x position of the center of the circle.
   * @param centerY The y position of the center of the circle.
   * @param radius The radius of the circle.
   * @param color The color of the circle lines.
   */
  static void drawCircleLines(float centerX, float centerY, float radius, Color color) {
    DrawCircleLines(centerX, centerY, radius, color);
  }
};
