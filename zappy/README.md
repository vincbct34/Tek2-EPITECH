# ZAPPY

## 📺 1. Présentation du Projet

**Zappy** est un projet de programmation réseau et de simulation temps réel, réalisé dans le cadre du module **B-YEP-400**. Il s’agit d’un jeu multijoueur en ligne où plusieurs équipes de clients IA s’affrontent sur une carte pour accomplir un objectif commun : faire évoluer **au moins 6 joueurs au niveau 8**.

### 🔧 Binaries

* `zappy_server` : serveur central (C)
* `zappy_gui` : client graphique (C++)
* `zappy_ai` : client IA autonome (langage libre)

### 🌟 Objectif du jeu

* Évoluer jusqu’au **niveau 8** grâce à des rituels d'incantation collectifs
* Survivre en gérant la nourriture (1 food = 126 unités de temps)
* Explorer un monde torique avec ressources réparties

### 🧱 Ressources disponibles

* `food`, `linemate`, `deraumere`, `sibur`, `mendiane`, `phiras`, `thystame`

### 🌐 Règles principales

* Le serveur gère la carte, les joueurs, et les ressources
* Le GUI s’authentifie avec le mot-clé `GRAPHIC`
* Les clients IA sont autonomes une fois lancés
* Le serveur utilise `poll` pour le multiplexage

---

## 🔴 2. Protocoles Réseau

### 📡 a. Protocole Server ↔ AI

#### ⚖️ Connexion initiale

```
<-- WELCOME
--> TEAM_NAME
<-- CLIENT_NUM
<-- X Y
```

#### ✅ Commandes IA

| Commande         | Description                 | Temps | Réponse                     |
| ---------------- | --------------------------- | ----- | --------------------------- |
| `Forward`        | Avancer                     | 7/f   | `ok`                        |
| `Right` / `Left` | Tourner à 90°               | 7/f   | `ok`                        |
| `Look`           | Vision                      | 7/f   | `[tile1, tile2, ...]`       |
| `Inventory`      | Voir son inventaire         | 1/f   | `[food 5, ...]`             |
| `Broadcast`      | Envoyer un message          | 7/f   | `ok`                        |
| `Connect_nbr`    | Slots disponibles           | -     | `nb`                        |
| `Fork`           | Ponte d’un oeuf             | 42/f  | `ok`                        |
| `Eject`          | Éjecte les autres joueurs   | 7/f   | `ok` / `ko`                 |
| `Take object`    | Ramasse un objet            | 7/f   | `ok` / `ko`                 |
| `Set object`     | Dépose un objet             | 7/f   | `ok` / `ko`                 |
| `Incantation`    | Lance un rituel d'élévation | 300/f | `Elevation underway` / `ko` |

* Jusqu'à 10 commandes sans réponse possible
* Tout ordre invalide : `ko`

---

### 💻 b. Protocole Server ↔ GUI

#### 📡 Côté Serveur → GUI

| Message          | Description                              |
| ---------------- | ---------------------------------------- |
| `msz X Y`        | Taille de la carte                       |
| `bct X Y q0..q6` | Contenu d'une case                       |
| `mct`            | Contenu de toute la carte                |
| `tna N`          | Nom des équipes                          |
| `pnw`            | Nouveau joueur connecté                  |
| `ppo`            | Position du joueur                       |
| `plv`            | Niveau du joueur                         |
| `pin`            | Inventaire du joueur                     |
| `pex`            | Expulsion                                |
| `pbc`            | Message broadcast                        |
| `pic`            | Début d’incantation                      |
| `pie`            | Fin d’incantation                        |
| `pfk`            | Ponte d’oeuf par un joueur               |
| `pgt` / `pdr`    | Prise / Dépôt de ressource               |
| `pdi`            | Mort du joueur                           |
| `enw`            | Oeuf pondu                               |
| `ebo`            | Connexion via un oeuf                    |
| `edi`            | Mort d’un oeuf                           |
| `sgt`            | Récupération de la fréquence             |
| `sst`            | Modification de la fréquence             |
| `seg`            | Fin de partie                            |
| `smg`            | Message du serveur                       |
| `suc` / `sbp`    | Commande inconnue / Paramètres invalides |

#### 📢 Côté GUI → Serveur

* `msz`, `bct`, `mct`, `tna`, `ppo #n`, `plv #n`, `pin #n`, `sgt`, `sst T`

---

## 🔹 3. Architecture Technique

### 📂 Structure du projet

```
.
├── include/           # Fichiers d'en-tête partagés
├── server/            # Code source du serveur C
├── gui/               # Code source de la GUI C++ (SFML)
├── ai/                # Client IA
├── tests/             # Scénarios de test
├── Makefile           # Compilation des 3 binaires
```

### ⚒️ Outils recommandés

* `valgrind` pour les fuites mémoire
* `strace` pour vérifier le bon usage de `poll`
* `telnet` pour debug protocole

### 📆 Exemple de lancement

```bash
make zappy_server zappy_gui zappy_ai

./zappy_server -p 4242 -x 10 -y 10 -n Team1 Team2 -c 5 -f 100
./zappy_gui -p 4242 -h localhost
./zappy_ai -p 4242 -n Team1 -h localhost
```

---

## 👥 4. Contributeurs

- Vincent B. (vincbct34)
- Noah L. (alpha0344)
- Axel P. (Speedy)
- Pierre L. (pierrelmy)