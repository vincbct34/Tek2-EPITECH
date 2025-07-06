# 🎮 Arcade - A Retro Gaming Platform

Bienvenue sur **Arcade**, une plateforme rétro de jeux en C++ qui vous permet de jouer à différents classiques via des bibliothèques graphiques interchangeables dynamiquement.

---

## 📌 Présentation

Arcade est une application modulaire permettant :
- Le **chargement dynamique** de bibliothèques graphiques et de jeux.
- L'affichage d’un **menu interactif** listant tous les jeux et bibliothèques disponibles.
- Le **changement de moteur graphique ou de jeu à la volée** pendant l’exécution.
- La **sauvegarde des scores** des joueurs.

---

## ⚙️ Fonctionnalités principales

- Chargement dynamique via `dlopen`, `dlsym`, `dlclose`, `dlerror`
- Interface intuitive avec :
  - Liste des jeux disponibles
  - Liste des interfaces graphiques disponibles
  - Saisie du pseudo joueur
  - Affichage des meilleurs scores
- Navigation par clavier :
  - Changer de jeu ou de moteur graphique
  - Relancer une partie
  - Retour au menu principal
  - Quitter le programme proprement

---

## 🧩 Arborescence du projet

```
.
├── arcade                  # Binaire principal
├── lib/                    # Dossiers contenant les .so (jeux et interfaces)
│   ├── arcade_ncurses.so
│   ├── arcade_sdl2.so
│   ├── arcade_sfml.so
│   ├── arcade_snake.so
│   └── arcade_centipede.so
├── src/                    # Code source
│   ├── core/               # Cœur de l’application
│   │── games/              # Implémentation des jeux
│   │── utils/              # Gestion des Event et des Entités
│   └── graphicals/         # Implémentation des interfaces graphiques
├── doc/                    # Documentation technique et d'intégration
├── Makefile                # Compilation
└── README.md
```

---

## 🛠️ Compilation

### ▶️ Via Makefile

```bash
make           # Compile tout (core + jeux + graphismes)
make core      # Compile uniquement le cœur de l'application
make games     # Compile uniquement les jeux
make graphicals # Compile uniquement les interfaces graphiques
make clean     # Supprime les fichiers objets
make fclean    # Supprime tous les binaires et objets
make re        # fclean + all
```

---

## ▶️ Lancement

```bash
./arcade ./lib/arcade_sfml.so
```

- Le seul argument obligatoire est le chemin vers une bibliothèque graphique valide.
- En cas d’erreur (aucun argument, bibliothèque invalide), un message explicite s'affiche et le programme retourne `84`.

---

## 🎮 Jeux disponibles

Parmi les jeux implémentés dans ce dépôt :

- ✅ Snake
- ✅ Centipede

---

## 🎨 Bibliothèques graphiques disponibles

Le projet inclut les moteurs graphiques suivants :

- ✅ nCurses
- ✅ SDL2
- ✅ SFML (exemple additionnel)

---

## 📄 Documentation

Vous trouverez dans le dossier `./doc/` :

- Une documentation complète

---

## 🤝 Collaboration inter-groupes

Les interfaces `IGame.hpp` et `IGraphical.hpp` sont partagées avec d’autres groupes pour assurer l’interopérabilité entre les jeux et interfaces.

```
🧑‍🤝‍🧑 Interface partagée avec :
> Emilio Manca – emilio.manca@epitech.eu
```

---

## 👥 Équipe

- Vincent Bichat – [vincent.bichat@epitech.eu](mailto:vincent.bichat@epitech.eu)
- Noah Lamont – [noah.lamont@epitech.eu](mailto:noah.lamont@epitech.eu)
- Axel Pereto – [axel.pereto@epitech.eu](mailto:axel.pereto@epitech.eu)

---

Merci de votre lecture et bon jeu ! 🎉
