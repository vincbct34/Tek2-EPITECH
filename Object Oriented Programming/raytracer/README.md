# 🎇 Raytracer

> Projet B-OOP-400 — Raytracer en C++

## 🧠 Présentation

Ce projet consiste à développer un moteur de ray tracing en C++, capable de générer des images réalistes à partir de la description d'une scène dans un fichier de configuration. Le moteur repose sur la simulation du chemin inverse de la lumière pour déterminer la couleur de chaque pixel.

Le rendu final est généré sous la forme d’un fichier `.ppm`.

## 🚀 Compilation

Le projet se compile via un `Makefile` à la racine du dépôt.

```bash
make         # Compile le projet
make clean   # Supprime les fichiers objets
make fclean  # Supprime les fichiers objets ET le binaire
```

> Le binaire `raytracer` est généré à la racine du projet.

## 🖥️ Utilisation

```bash
./raytracer <SCENE_FILE>
```

* `<SCENE_FILE>` : chemin vers un fichier de configuration de scène au format `libconfig++`.

### Exemple :

```bash
./raytracer scenes/demo.cfg
```

L’image générée est produite sous forme de fichier `output.ppm`.

## 📦 Architecture

```
src/
├── core/              # Moteur principal
├── exceptions/        # Gestion des erreurs
├── loaders/           # Chargement des scènes et plugins
├── plugins/           # Primitives, lumières, rendu, caméra (via plugins dynamiques)
├── main.cpp           # Point d'entrée de l'application
```

## 🧩 Fonctionnalités

### ✅ Obligatoires ("Must")

* **Primitives** : Sphere, Plane
* **Transformations** : Translation
* **Lumières** : Directionnelle, Ambiante
* **Matériaux** : Couleur unie
* **Sortie** : Image `.ppm`
* **Fichier de scène** : Parsing via `libconfig++`

### ✅ Recommandées ("Should")

* **Primitives** : Cone, Cylinder
* **Transformations** : Rotation
* **Effets** : Ombres portées

### 🔁 Extensions implémentées (bonus)

* Chargement dynamique de plugins `.so`
* Architecture modulaire orientée interfaces
* Design Patterns utilisés :

  * **Factory** : pour la création des objets scène
  * **Composite** : pour la structure de la scène

## 📸 Format de la scène (`.cfg`)

Extrait d’un fichier de configuration :

```cfg
camera: {
  resolution = { width = 800; height = 600; };
  position = { x = 0; y = -100; z = 20; };
  rotation = { x = 0; y = 0; z = 0; };
  fieldOfView = 72.0;
};

primitives: {
  spheres = (
    { x = 0; y = 0; z = 0; r = 25; color = { r = 255; g = 0; b = 0; }; }
  );
  planes = (
    { axis = "Z"; position = -10; color = { r = 200; g = 200; b = 255; }; }
  );
};

lights: {
  ambient = 0.2;
  directional = (
    { x = 1; y = -1; z = -1; }
  );
};
```

## 🧪 Tests

Des scènes de test sont disponibles dans le dossier `scenes/`. Le résultat de l’exécution produit une image `.ppm` consultable avec un visualiseur d’images compatible.

## 📄 Documentation

La documentation du projet est disponible dans `doc/doc.pdf`. Elle a été générée avec **Doxygen**.

```bash
make doc   # Génère la documentation dans le dossier doc/
```

## 👥 Auteurs

* Noah Lamont
* Axel Pereto
* Vincent Bichat

## 📚 Références

* [Ray tracing - Wikipedia](https://en.wikipedia.org/wiki/Ray_tracing_%28graphics%29)
* [LibConfig++](https://hyperrealm.github.io/libconfig/)
* [PPM Format](https://netpbm.sourceforge.net/doc/ppm.html)
