from typing import Dict, Tuple, Optional, List
from dataclasses import dataclass
MAX_LEVEL = 8

class Pos:
    def __init__(self, x=0, y=0):
        self.x = x
        self.y = y

@dataclass
class GameState:
    """Class to represent the state of the game."""
    inventory: Dict[str, int]
    position: Optional[Tuple[int, int]]
    level: int
    food_units: int
    world_size: Tuple[int, int]
    team_name: str
    surroundings: List[str]
    
    zone_id: int = 0
    is_ready: bool = False
    is_joining: bool = False
    dispo_members: List[int] = None
    last_look: str = ""
    
    def __post_init__(self):
        if self.dispo_members is None:
            self.dispo_members = [0] * (MAX_LEVEL + 1)