# Comment créer un nouveau jeu dans Arcade

## Étape 1 : Créer un dossier pour le jeu
1. Naviguez vers le dossier `src/games/`.
2. Créez un nouveau dossier portant le nom de votre jeu (par exemple, `src/games/mygame/`).
3. Ajoutez les fichiers nécessaires pour votre jeu, comme `mygame.cpp` et `mygame.hpp`.

## Étape 2 : Modifier le Makefile
1. Ouvrez le Makefile principal du projet.
2. Ajoutez votre nouveau jeu dans les règles de compilation. Par exemple :
   ```makefile
   GAMES_SRCS += src/games/mygame/mygame.cpp
   ```

## Étape 3 : Créer les fichiers d'en-tête
1. Naviguez vers le dossier `src/include/games/`.
2. Créez un fichier d'en-tête pour votre jeu (par exemple, `mygame.hpp`).
3. Assurez-vous que toutes vos classes et fonctions appartiennent au namespace `Arcade`.

## Étape 4 : Implémenter la classe du jeu
1. Votre classe doit hériter de l'interface `IGame` située dans `src/include/games/IGame.hpp`.
2. Implémentez les fonctions suivantes dans votre classe :
   - `void init(std::vector<std::string>, std::vector<std::string>, int, int, std::unordered_map<std::string, std::array<std::pair<std::string, int>, 3>>) override;`
   - `void getInput(Event event) override;`
   - `void displayEndScreen() override;`
   - `const std::vector<std::unique_ptr<IEntity>> &getElement() const override;`
   - `int updateGame(Event event) override;`
   - `std::string getChosenGraphical() const override;`
   - `std::string getChosenGame() const override;`
   - `std::string getGameName() const override;`

## Étape 5 : Description des fonctions
### `init`
- Cette fonction initialise les éléments nécessaires au bon fonctionnement du jeu.
- Exemple d'éléments à initialiser :
  - Le score.
  - Le nombre d'ennemis.
  - La classe `_elements` qui contient tous les éléments graphiques (formes, sprites, etc.).

### `getInput`
- Cette fonction reçoit un objet `Event` qui contient des informations sur la touche pressée par l'utilisateur.

### `getElement`
- Cette fonction retourne la classe `_elements`, qui contient tous les éléments graphiques du jeu.

### `updateGame`
- Cette fonction met à jour l'état du jeu à chaque tick (par exemple, déplacement des personnages, gestion des collisions, etc.).

### `displayEndScreen`
- Cette fonction affiche l'écran de fin lorsque le jeu est terminé.

## Étape 6 : Exemple minimal
Voici un exemple minimal de classe de jeu :
```cpp
#include "IGame.hpp"

namespace Arcade {
    class MyGame : public IGame {
        public:
            void init(std::vector<std::string>, std::vector<std::string>, int, int, std::unordered_map<std::string, std::array<std::pair<std::string, int>, 3>>) override;
            void getInput(Event event) override;
            void displayEndScreen() override;
            const std::vector<std::unique_ptr<IEntity>> &getElement() const override;
            int updateGame(Event event) override;
            std::string getChosenGraphical() const override;
            std::string getChosenGame() const override;
            std::string getGameName() const override;

        private:
            std::vector<std::unique_ptr<IEntity>> _elements;
            int _score = 0;
    };
}
```

## Étape 7 : Compiler et tester
1. Compilez le projet en utilisant le Makefile.
2. Lancez Arcade et vérifiez que votre jeu apparaît dans la liste des jeux disponibles.
3. Testez votre jeu pour vous assurer qu'il fonctionne correctement.
