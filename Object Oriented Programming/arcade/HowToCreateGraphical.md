    # How to Create a Graphical Library for Arcade

This document provides a guide on how to create a graphical library for the Arcade project. The graphical library must implement certain mandatory functions to ensure compatibility with the Arcade framework.

## Mandatory Functions

### 1. Initialization and Cleanup

- **Initialization**: The graphical library must provide a function to initialize the graphical context, load necessary resources, and prepare the window for rendering.
- **Cleanup**: The graphical library must provide a function to clean up resources, destroy the window, and properly shut down the graphical context.

### 2. Window Management

- **Get Window Size**: The graphical library must provide a function to return the current window size as a pair of integers representing the width and height.

### 3. Text Input

- **Get Player Name**: The graphical library must provide a function to capture text input from the user, specifically to get the player's name. This function should handle text input events and return the entered name as a string.

### 4. Event Handling

- **Poll Events**: The graphical library must implement a function to handle and process events such as keyboard and mouse inputs. This function should ensure that the application responds to user interactions appropriately.

### 5. Rendering

- **Render Text**: The graphical library should provide a function to render text onto the screen. This involves creating a texture from a text surface and displaying it at a specified location within the window.

- **Present Renderer**: The graphical library must include a function to update the display with the latest rendered content. This function should be called after all rendering operations are completed to present the final output to the user.
