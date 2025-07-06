from enum import Enum

class Priority(Enum):
    CRITICAL = 1
    HIGH = 2
    MEDIUM = 3
    LOW = 4

VISION_RANGES = [(0, 0), (1, 3), (4, 8), (9, 15), (16, 24), (25, 36), (37, 51), (52, 69), (70, 90)]
STONES = ["linemate", "deraumere", "sibur", "mendiane", "phiras", "thystame"]
MAX_LEVEL = 8
MIN_FOOD_THRESHOLD = 120

MAX_STUFF = {
    "linemate": 15,  # Augmenté car très utilisé
    "deraumere": 12, # Augmenté pour élévations multiples
    "sibur": 15,     # Augmenté car très demandé
    "mendiane": 8,   # Augmenté légèrement
    "phiras": 10,    # Augmenté pour niveaux élevés
    "thystame": 3    # Augmenté car rare et précieux
}

ELEVATION_REQUIREMENTS = {
    1: {"players": 1, "linemate": 1},
    2: {"players": 2, "linemate": 1, "deraumere": 1, "sibur": 1},
    3: {"players": 2, "linemate": 2, "sibur": 1, "phiras": 2},
    4: {"players": 4, "linemate": 1, "deraumere": 1, "sibur": 2, "phiras": 1},
    5: {"players": 4, "linemate": 1, "deraumere": 2, "sibur": 1, "mendiane": 3},
    6: {"players": 6, "linemate": 1, "deraumere": 2, "sibur": 3, "phiras": 1},
    7: {"players": 6, "linemate": 2, "deraumere": 2, "sibur": 2, "mendiane": 2, "phiras": 2, "thystame": 1}
}