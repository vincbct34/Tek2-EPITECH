import logging
from enum import Enum
from typing import Dict, List

DEBUG = True

class LogLevel(Enum):
    DEBUG = logging.DEBUG
    INFO = logging.INFO
    WARNING = logging.WARNING
    ERROR = logging.ERROR

def setup_logger(name: str, level: LogLevel = LogLevel.DEBUG):
    """
    Setup a logger with proper formatting for the Zappy AI.
    """
    logger = logging.getLogger(name)
    logger.setLevel(level.value if DEBUG else logging.WARNING)
    
    if not logger.handlers:
        handler = logging.StreamHandler()
        formatter = logging.Formatter(
            '%(asctime)s | %(name)s | %(levelname)s | %(message)s',
            datefmt='%H:%M:%S'
        )
        handler.setFormatter(formatter)
        logger.addHandler(handler)
    
    return logger

def print_debug(message: str):
    """
    Print a debug message if DEBUG is set to True.
    """
    if DEBUG:
        print(message)

def find_closest_food(surroundings: List[str]) -> Dict:
    """Trouve la nourriture la plus proche dans le champ de vision"""
    food_info = {"found": False, "case": 0}
    
    for case_idx, tile in enumerate(surroundings):
        objects = tile.split() if tile else []
        if "food" in objects:
            food_info["found"] = True
            food_info["case"] = case_idx
            return food_info
    
    return food_info

def find_closest_resource(surroundings: List[str], resources: List[str]) -> Dict:
    """Trouve la ressource la plus proche parmi une liste de ressources"""
    resource_info = {"found": False, "case": 0}

    for case_idx, tile in enumerate(surroundings):
        objects = tile.split() if tile else []
        for resource in resources:
            if resource in objects:
                resource_info["found"] = True
                resource_info["case"] = case_idx
                return resource_info
    
    return resource_info

def aggressive_food_search(client, logger, turn_counter):
    """Recherche agressive de nourriture avec mouvements optimisés"""
    from parsers import parse_look_response, have_seen_stones
    from movement import go_to_case
    
    logger.info('🚨 AGGRESSIVE FOOD SEARCH ACTIVATED')
    
    directions = ["Forward", "Right", "Forward", "Right", "Forward", "Forward", "Left", "Forward", "Left"]
    
    for direction in directions:
        client.send_command(direction)
        response = client.receive()

        client.send_command("Look")
        look_response = client.receive()
        if look_response.startswith("["):
            surroundings = parse_look_response(look_response)
            food_info = have_seen_stones(surroundings, ["food"])
            if food_info["found"]:
                logger.info('🎯 Found food during aggressive search!')
                go_to_case(client, logger, food_info["case"])
                break
        
        if response != "ok":
            break
            
    return turn_counter + 1
