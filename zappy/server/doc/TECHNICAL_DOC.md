# Documentation Technique — Zappy Server

## Table des matières
- [Introduction](#introduction)
- [Architecture Générale](#architecture-générale)
- [Structures de Données Principales](#structures-de-données-principales)
- [Processus de Lancement](#processus-de-lancement)
- [Boucle Principale et Gestion des Événements](#boucle-principale-et-gestion-des-événements)
- [Gestion des Clients](#gestion-des-clients)
- [Gestion du Jeu](#gestion-du-jeu)
- [Commandes et Protocoles](#commandes-et-protocoles)
- [Gestion des Ressources et Incantations](#gestion-des-ressources-et-incantations)
- [Annexes](#annexes)

---

## Introduction
Cette documentation détaille l'architecture et le fonctionnement du serveur du projet Zappy. Elle présente les principales structures, le déroulement du process serveur, la gestion des clients, du jeu, des ressources et des commandes.

## Architecture Générale
Le serveur est découpé en plusieurs modules :
- **args-parsing/** : gestion des arguments et configuration serveur
- **game/** : logique de jeu, gestion des joueurs, ressources, incantations, œufs, etc.
- **network/** : gestion des commandes et communication réseau
- **client/** : gestion des commandes clients
- **commands/** : implémentation des commandes AI/GUI
- **utils/** : utilitaires, boucle principale, gestion du temps

## Structures de Données Principales
### server_config_t
Décrit la configuration du serveur (port, taille de la map, équipes, slots, fréquence, etc).

### game_state_t
Structure centrale contenant l’état du jeu :
- `server_config_t *config` : configuration serveur
- `tile_t **map` : grille de la map
- `player_t *players` : tableau de joueurs
- `egg_t *eggs` : tableau d’œufs
- `team_t *teams` : équipes
- `client_t *clients` : liste chaînée de clients
- `incantation_ritual_t active_rituals[32]` : incantations en cours
- Divers compteurs et timestamps

### player_t, egg_t, team_t, client_t
Structures représentant respectivement un joueur, un œuf, une équipe et un client connecté.

### tile_t
Représente une case de la map et contient un tableau de ressources.

## Processus de Lancement
1. **Parsing des arguments** : via `parse_args` (args-parsing/args_parser.c)
2. **Initialisation de la configuration** : `init_config`, puis allocation de la structure `server_config_t`
3. **Initialisation de l’état du jeu** : `init_game_state` (game/game_state.c)
   - Allocation de la map, des équipes, initialisation des ressources, incantations, etc.
4. **Création du socket serveur** : `setup_server` (utils/run_logic.c)
5. **Entrée dans la boucle principale** : gestion des événements via `poll`, acceptation des connexions, gestion des clients et du jeu.

## Boucle Principale et Gestion des Événements
- Utilisation de `poll` pour surveiller le socket serveur et les clients.
- Fonctions clés :
  - `handle_server_event` : accepte les nouvelles connexions
  - `handle_poll_events` : dispatch des événements (nouveau client, message client)
  - `handle_client_event` : gestion d’un événement sur un client
  - `process_client_input` : traitement des commandes reçues
  - `update_game_state` : mise à jour logique du jeu (ressources, incantations, œufs, etc)

## Gestion des Clients
- Ajout : `add_client`, suppression : `remove_client`, recherche : `find_client`
- Authentification : gestion de l’équipe, création du joueur (`create_player_for_client`)
- Bufferisation des commandes : chaque client possède un buffer circulaire de commandes (`command_buffer_entry_t`)
- Traitement des commandes prêtes : `process_ready_commands`

## Gestion du Jeu
- **Map** : allocation dynamique, chaque case = `tile_t` (ressources)
- **Joueurs** : tableau dynamique, chaque joueur a position, inventaire, orientation, niveau, etc.
- **Équipes** : slots disponibles, nom, etc.
- **Œufs** : gestion de la ponte et de l’éclosion
- **Incantations** : gestion des rituels, vérification des conditions, montée de niveau
- **Ressources** : spawn cyclique, ramassage, dépôt

## Commandes et Protocoles
- **AI** : commandes Forward, Right, Left, Look, Inventory, Broadcast, Connect_nbr, Fork, Eject, Take, Set, Incantation…
  - Chaque commande : struct `ai_command_t` (nom, temps d’exécution, callback)
- **GUI** : commandes msz, bct, mct, tna, ppo, plv, pin, sgt, sst…
  - struct `gui_command_t`
- **Gestion** : parsing, bufferisation, exécution différée ou immédiate selon le type

## Gestion des Ressources et Incantations
- **Ressources** : spawn initial (`initialize_map_resources`), respawn cyclique (`spawn_resources_cycle`), ajout/retrait sur une case
- **Incantations** : gestion des rituels (`incantation_ritual_t`), vérification des conditions, montée de niveau, notifications GUI

## Annexes
- Voir les headers pour le détail des structures et prototypes
- Les modules sont fortement découplés pour faciliter la maintenance et l’évolution
- Pour plus de détails sur chaque fonction, se référer aux fichiers d’implémentation et headers associés
