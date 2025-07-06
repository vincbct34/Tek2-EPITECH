# 📝 MyPandoc

**MyPandoc** est un convertisseur de documents entre trois formats courants : **Markdown**, **JSON** et **XML**. Le programme permet de lire un fichier dans un format donné, d’analyser sa structure (header, body, sections, etc.), puis de l’exporter vers un autre format.

Ce projet a été réalisé dans le cadre de l’enseignement à EPITECH, avec un accent mis sur la conception de parsers personnalisés en **Haskell**.

---

## 📆 Formats supportés

### ✅ Formats d’entrée et de sortie :

* `markdown` — Fichier avec syntaxe Markdown standard (titres, paragraphes, listes, etc.)
* `json` — Représentation structurée en JSON d’un document
* `xml` — Représentation arborescente en XML du même contenu

Chaque format est convertible dans un autre.

---

## 🚀 Utilisation

### 1. Exécutable CLI

Compilation via `make stack` puis exécution du programme :

```bash
./mypandoc -i input.md -f json           # Convertit Markdown → JSON (affiche sur stdout)
./mypandoc -i input.json -f markdown -o out.md   # Convertit JSON → Markdown
./mypandoc -i input.xml -f json -e xml   # Forçage du format d’entrée
```

**Arguments disponibles :**

* `-i ifile` : chemin vers le fichier d’entrée
* `-f oformat` : format de sortie (`json`, `xml`, `markdown`)
* `-o ofile` *(optionnel)* : fichier de sortie (stdout par défaut)
* `-e iformat` *(optionnel)* : format d’entrée si non détectable automatiquement

### 2. API Web (bonus)

Une API RESTful est disponible via `Scotty`. Lancer avec :

```bash
cd backend
````
```bash
make re  # ou stack build
```
```bash
./mypandoc
```

Appel POST possible avec `curl` :

```bash
curl -X POST http://localhost:3000/api/convert \
  -F "file=@./test_files/syntaxe.json" \
  -F "format=markdown"
```

Cela renvoie le contenu converti au format demandé.

---

## 💧 Fonctionnalités

* Parsers personnalisés basés sur une monade `Parser`
* Gestion fine des éléments de document : paragraphes, sections, listes, titres, etc.
* Compatibilité multi-formats avec conversion réversible
* API REST simple pour usage dans une interface Web

---

## 🔗 Chemins utiles

* Documentation :
```bash
make doc  # ou stack haddock
```
```bash
cd .stack-work/install/x86_64-linux/d7e9a64789873c1717dc8f3204d606c873cf9b0ded15a964c477e0442c78c80f/9.8.4/doc/index.html
```
* Sources principales : `src/`
* Tests & exemples : `test_files/`
* Serveur API : `bonus/backend/`

---

## 🎓 Projet EPITECH

Projet réalisé en 2e année dans le cadre de l'UE **FUN**. Objectifs principaux : parser à la main, comprendre les formats de documents, et structurer une base d’exportation multi-format propre et maintenable.
