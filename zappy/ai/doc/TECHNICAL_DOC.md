# Documentation Technique — Zappy AI

## Table des matières
- [Introduction](#introduction)
- [Architecture Générale](#architecture-générale)
- [Structures et Modules Principaux](#structures-et-modules-principaux)
- [Processus de Lancement](#processus-de-lancement)
- [Boucle Principale et Logique de l’Agent](#boucle-principale-et-logique-de-lagent)
- [Gestion de la Communication Réseau](#gestion-de-la-communication-réseau)
- [Gestion de l’État du Jeu](#gestion-de-letat-du-jeu)
- [Gestion des Actions et de la Stratégie](#gestion-des-actions-et-de-la-stratégie)
- [Annexes](#annexes)

---

## Introduction
Cette documentation détaille l’architecture et le fonctionnement de l’IA (client agent) du projet Zappy. Elle présente les modules, le process de lancement, la gestion de la communication, de l’état du jeu et de la logique décisionnelle.

## Architecture Générale
L’IA est structurée en plusieurs modules :
- **agent.py** : logique principale de l’agent, boucle de décision
- **client.py** : gestion de la connexion réseau, envoi/réception des commandes
- **communication.py** : parsing et envoi des messages serveur
- **game_state.py** : représentation locale de l’état du jeu
- **movement.py** : gestion des déplacements
- **elevation.py** : gestion des incantations et montées de niveau
- **parsers.py** : parsing des réponses serveur
- **utils.py** : fonctions utilitaires
- **constants.py** : constantes du protocole et du jeu

## Structures et Modules Principaux
### Agent
Classe principale orchestrant la logique de l’IA : prise de décision, gestion de l’état, interaction avec le serveur.

### Client
Gère la connexion TCP, l’envoi et la réception des commandes, la gestion du buffer réseau.

### GameState
Représente l’état local du jeu : inventaire, position, vision, niveau, etc.

### Communication
Gère le parsing des messages serveur, la construction des commandes à envoyer.

## Processus de Lancement
1. **Chargement de la configuration** (arguments, équipe, etc.)
2. **Connexion au serveur** via `Client`
3. **Initialisation de l’état local** (`GameState`)
4. **Entrée dans la boucle principale** : perception, décision, action

## Boucle Principale et Logique de l’Agent
- Boucle dans `main.py` ou `agent.py`
- À chaque itération :
  1. Lecture des messages serveur
  2. Mise à jour de l’état local (`GameState`)
  3. Prise de décision (stratégie, priorités)
  4. Envoi de la commande choisie
- Gestion des priorités : survie (food), montée de niveau, exploration, coopération

## Gestion de la Communication Réseau
- **Client** : gestion du socket, envoi/réception asynchrone
- **Communication** : parsing des réponses, gestion des commandes en attente
- **Parsers** : extraction des informations utiles (inventaire, vision, etc.)

## Gestion de l’État du Jeu
- **GameState** : inventaire, position, vision, niveau, état des autres joueurs
- Mise à jour à chaque message pertinent du serveur
- Gestion des objectifs (ressources, incantations, etc.)

## Gestion des Actions et de la Stratégie
- **Movement** : fonctions de déplacement, pathfinding simple
- **Elevation** : gestion des incantations, vérification des conditions
- **Stratégie** : choix de l’action optimale selon l’état du jeu
- Gestion de la coopération avec les autres IA (broadcast, etc.)

## Annexes
- Voir les modules pour le détail des classes et fonctions
- L’architecture est pensée pour être modulaire et facilement extensible
- Pour plus de détails sur chaque fonction, se référer aux fichiers sources et à la documentation inline
