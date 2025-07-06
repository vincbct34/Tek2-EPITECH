# Documentation Technique — Zappy GUI

## Table des matières
- [Introduction](#introduction)
- [Architecture Générale](#architecture-générale)
- [Structures de Données Principales](#structures-de-données-principales)
- [Processus de Lancement](#processus-de-lancement)
- [Boucle Principale et Rendu](#boucle-principale-et-rendu)
- [Gestion du Réseau](#gestion-du-réseau)
- [Gestion de l’Affichage et des Événements](#gestion-de-laffichage-et-des-événements)
- [Gestion des Joueurs, Tuiles et Ressources](#gestion-des-joueurs-tuiles-et-ressources)
- [Annexes](#annexes)

---

## Introduction
Cette documentation détaille l’architecture et le fonctionnement du client graphique (GUI) du projet Zappy. Elle présente les principales structures, le process de lancement, la gestion du réseau, du rendu, des joueurs et des ressources.

## Architecture Générale
Le GUI est structuré en plusieurs modules :
- **src/core/** : logique principale, boucle de rendu, gestion du jeu
- **src/network/** : gestion du client réseau, parsing des paquets
- **src/utils/** : utilitaires, wrappers Raylib, gestion des exceptions
- **assets/** : ressources graphiques (sprites, images)

## Structures de Données Principales
### Game
Classe centrale gérant l’état du jeu, la map, les joueurs, les équipes, les ressources, etc.

### Player, Egg, Team, Tile
- **Player** : position, orientation, inventaire, niveau, équipe, etc.
- **Egg** : position, équipe, état d’éclosion
- **Team** : nom, couleur, joueurs associés
- **Tile** : ressources présentes, coordonnées, joueurs présents

### NetworkClient
Gère la connexion au serveur, l’envoi et la réception des paquets, le parsing des commandes GUI.

## Processus de Lancement
1. **Chargement des assets** : images, sprites, polices
2. **Initialisation du réseau** : création du client, connexion au serveur (`NetworkClient`)
3. **Initialisation du jeu** : création de la map, des équipes, des joueurs
4. **Entrée dans la boucle principale** : gestion des événements, rendu, mise à jour de l’état du jeu

## Boucle Principale et Rendu
- Boucle principale dans `main.cpp`/`Game.cpp`
- Rendu graphique avec Raylib (ou wrapper custom)
- Gestion des événements clavier/souris
- Mise à jour de l’état du jeu à chaque frame
- Affichage des tuiles, joueurs, ressources, œufs, messages, etc.

## Gestion du Réseau
- **NetworkClient** : connexion TCP, réception asynchrone des paquets
- Parsing des commandes serveur (bct, pnw, ppo, plv, pin, etc.)
- Mise à jour de l’état du jeu en fonction des messages reçus
- Gestion des erreurs réseau et reconnexion

## Gestion de l’Affichage et des Événements
- **Component** : base pour les éléments affichables
- Fonctions de dessin : draw_components, draw_players, draw_tiles, draw_elevations, etc.
- Affichage dynamique selon l’état du jeu (victoire, défaite, déconnexion…)
- Gestion des entrées utilisateur (navigation, zoom, sélection, etc.)

## Gestion des Joueurs, Tuiles et Ressources
- Mise à jour des positions et états des joueurs selon les paquets serveur
- Affichage des ressources sur chaque tuile
- Gestion des œufs (apparition, éclosion)
- Affichage des incantations, effets visuels

## Annexes
- Voir les headers et sources pour le détail des classes et méthodes
- Les modules sont conçus pour être découplés et facilement maintenables
- Pour plus de détails sur chaque fonction, se référer aux fichiers d’implémentation et headers associés
