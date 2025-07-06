/*
** EPITECH PROJECT, 2025
** B-OOP-400-MPL-4-1-arcade-vincent.bichat
** File description:
** IGraphical
*/

#pragma once

#include "Entity.hpp"
#include "Event.hpp"

#include <iostream>
#include <string>
#include <vector>

namespace Arcade {

    class Event;
    class IEntity;

class IGraphical {
    public:
        virtual ~IGraphical() = default;

        virtual void init() = 0; // Initialisation de la bibliothèque
        virtual void clearScreen() = 0; // Effacer l'écran
        virtual void drawElements(const std::vector<std::unique_ptr<IEntity>> &elements) = 0; // Dessiner les éléments
        virtual void refresh() = 0; // Rafraîchir l'affichage
        virtual Event getInput(const std::vector<std::unique_ptr<IEntity>> &elements) = 0; // Récupérer une entrée clavier
        virtual void stop() = 0; // Fermeture de la bibliothèque
        virtual std::pair<int, int> getWindowSize() const = 0; // Récupérer la taille de la fenêtre
        virtual bool canUpdate() = 0; // Vérifier si la bibliothèque peut être mise à jour
        virtual std::string getPlayerName() = 0;
};

} // namespace Arcade