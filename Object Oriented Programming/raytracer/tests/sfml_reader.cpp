#include <SFML/Graphics.hpp>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

bool loadPPM_P3(const std::string& filename, sf::Image& image) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        std::cerr << "Erreur: impossible d'ouvrir " << filename << std::endl;
        return false;
    }

    std::string magicNumber;
    file >> magicNumber;
    if (magicNumber != "P3") {
        std::cerr << "Erreur: format PPM non supporté (besoin de P3)." << std::endl;
        return false;
    }

    int width = 0, height = 0, maxColorValue = 0;
    file >> width >> height >> maxColorValue;

    if (maxColorValue != 255) {
        std::cerr << "Erreur: Seuls les PPM avec max color 255 sont supportés." << std::endl;
        return false;
    }

    file.ignore();  // Ignorer la fin de ligne après les dimensions.

    std::vector<sf::Uint8> pixels(width * height * 4); // RGBA

    int r, g, b;
    for (int i = 0; i < width * height; ++i) {
        file >> r >> g >> b;
        pixels[i * 4 + 0] = r;  // R
        pixels[i * 4 + 1] = g;  // G
        pixels[i * 4 + 2] = b;  // B
        pixels[i * 4 + 3] = 255; // A (alpha à 255)
    }

    image.create(width, height, pixels.data());
    return true;
}

int main() {
    sf::RenderWindow window(sf::VideoMode(800, 600), "PPM P3 Loader");

    sf::Image img;
    if (!loadPPM_P3("image.ppm", img)) {
        return -1;
    }

    sf::Texture texture;
    texture.loadFromImage(img);

    sf::Sprite sprite(texture);
    //sprite.setScale(100.0f / sprite.getLocalBounds().width, 100.0f / sprite.getLocalBounds().height);  // Redimensionner

    while (window.isOpen()) {
        sf::Event event;
        while (window.pollEvent(event)) {
            if (event.type == sf::Event::Closed)
                window.close();
        }

        window.clear();  // Nettoyer la fenêtre
        window.draw(sprite);  // Dessiner le sprite
        window.display();  // Afficher
    }

    return 0;
}
