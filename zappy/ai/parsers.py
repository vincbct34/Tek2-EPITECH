import re
from typing import Dict, List

def parse_look_response(response: str) -> List[str]:
    """Parse the response from the 'look' command and return a list of tiles."""
    content = response[1:-1]
    tiles = content.split(',')
    return [tile.strip() for tile in tiles]

def parse_inventory_response(response: str) -> Dict[str, int]:
    """Parse the response from the 'inventory' command and return a dictionary of items."""
    inventory = {}
    if response.startswith("[") and response.endswith("]"):
        content = response[1:-1]
        matches = re.findall(r'(\w+)\s+(\d+)', content)
        for item, quantity in matches:
            inventory[item] = int(quantity)
    return inventory

def find_resource_in_view(surroundings: List[str], resource: str) -> int:
    """Return the INDEX of the tile containing the resource."""
    for i, tile in enumerate(surroundings):
        if resource in tile.split():
            return i
    return -1

def count_players_on_current_tile(surroundings: List[str]) -> int:
    """Count the number of players on the current tile (including self)."""
    if surroundings:
        current_tile = surroundings[0]
        objects = current_tile.split() if current_tile else []
        return objects.count("player")
    return 0

def have_seen_stones(surroundings: List[str], missing_stones: List[str]) -> Dict:
    """Search for missing stones in the field of vision - optimized for closest first."""
    infos = {"found": False, "case": 0, "resource": ""}
    
    # Priorité à la nourriture si elle est dans la liste
    if "food" in missing_stones:
        for case_idx, tile in enumerate(surroundings):
            objects = tile.split() if tile else []
            if "food" in objects:
                infos["found"] = True
                infos["case"] = case_idx
                infos["resource"] = "food"
                return infos
    
    # Ensuite chercher les autres ressources
    for case_idx, tile in enumerate(surroundings):
        objects = tile.split() if tile else []
        for stone in missing_stones:
            if stone != "food" and stone in objects:
                infos["found"] = True
                infos["case"] = case_idx
                infos["resource"] = stone
                return infos
    
    return infos