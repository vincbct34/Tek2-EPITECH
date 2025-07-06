import random
from typing import Tuple, List, Dict
from utils import setup_logger, LogLevel, find_closest_food, find_closest_resource, aggressive_food_search
from constants import Priority, STONES, MAX_LEVEL, MAX_STUFF, VISION_RANGES
from game_state import GameState, Pos
from parsers import parse_look_response, parse_inventory_response, have_seen_stones
from movement import go_to_case, smart_exploration, move_to_resource, explore_for_resources
from elevation import (
    get_needed_resources_for_elevation, can_attempt_elevation, attempt_elevation,
    enough_are_ready, start_gathering, ELEVATION_REQUIREMENTS
)
from communication import (
    broadcast_message, check_for_broadcasts, handle_ready_msg, 
    handle_start_msg, handle_zone_msg
)

class Agent:
    def __init__(self, client):
        """Initialize the agent with a client connection."""
        self.client = client
        self.logger = setup_logger('AGENT', LogLevel.DEBUG)

        world_size = client.get_world_size()
        self.game_state = GameState(
            inventory={},
            position=None,
            level=1,
            food_units= 10,
            world_size=world_size,
            team_name=client.team_name,
            surroundings=[]
        )

        self.elevation_requirements = ELEVATION_REQUIREMENTS
        
        self.msg_functions = {
            "ready": lambda msg: handle_ready_msg(self.game_state, self.logger, msg),
            "start": lambda msg: handle_start_msg(self.game_state, self.logger, msg),
            "zone": lambda msg: handle_zone_msg(self.game_state, self.logger, msg)
        }
        
        self.turn_counter = 0

    def think(self):
        """Main thinking cycle of the agent."""
        self.logger.info('🧠 Starting AI think cycle')

        self.update_game_state()
        action = self.decide_action()

        self.logger.info(f'🎯 Decision made: {action[0]} (Priority: {action[1].name})')
        self.execute_action(action)

    def update_game_state(self):
        """Update the game state with the latest information from the server."""
        self.logger.debug('📊 Updating game state...')
        
        self.client.send_command("Look")
        surroundings_raw = self.client.receive()
        if not surroundings_raw.startswith("["):
            self.logger.warning(f'❌ Invalid Look response: {surroundings_raw}')
            return
        self.game_state.surroundings = parse_look_response(surroundings_raw)
        self.logger.debug(f'👀 Surroundings updated: {len(self.game_state.surroundings)} tiles visible')

        self.client.send_command("Inventory")
        inventory_raw = self.client.receive()
        self.game_state.inventory = parse_inventory_response(inventory_raw)
        self.logger.debug(f'🎒 Inventory: {self.game_state.inventory}')

        self.game_state.food_units = self.game_state.inventory.get("food", 0)
        self.logger.debug(f'🍯 Food units: {self.game_state.food_units}')

    
    def decide_action(self):
        """
        Enhanced decision-making logic inspired by Zappy-master
        """
        self.logger.debug('🤔 Analyzing situation for decision making...')

        self.take_everything_on_tile()

        if self.game_state.food_units < 15:
            self.logger.warning(f'⚠️ FOOD LOW! Only {self.game_state.food_units} units remaining')
            
            food_info = find_closest_food(self.game_state.surroundings)
            if food_info["found"]:
                self.logger.info(f'🍯 Food spotted at case {food_info["case"]} - moving towards it')
                return (f"move_to_case_{food_info['case']}", Priority.CRITICAL)
            else:
                self.logger.info('🔍 No food visible - active food search')
                return ("search_food_aggressive", Priority.CRITICAL)
        
        if can_attempt_elevation(self.game_state, self.logger):
            self.logger.info('🚀 All resources ready for elevation!')
            
            if not self.game_state.is_ready and enough_are_ready(self.game_state):
                self.logger.info('🎪 I am the last ready - starting gathering!')
                self.game_state.is_ready = True
                start_gathering(self.client, self.game_state, self.logger)
                return ("gathering_started", Priority.HIGH)
            elif not self.game_state.is_ready:
                self.logger.info('📢 Broadcasting ready status')
                broadcast_message(self.client, self.logger, f"ready:{self.game_state.level}")
                self.game_state.is_ready = True
                return ("wait_for_gathering", Priority.HIGH)

        if self.should_fork():
            self.logger.info('🍴 Attempting to fork!')
            return ("fork", Priority.HIGH)
        
        if self.game_state.level < MAX_LEVEL:
            missing_resources = get_needed_resources_for_elevation(self.game_state)
            
            if missing_resources:
                self.logger.debug(f'📋 Still need: {missing_resources}')
                
                best_resource_info = find_closest_resource(self.game_state.surroundings, missing_resources)
                if best_resource_info["found"]:
                    self.logger.info(f'� Found needed resource at case {best_resource_info["case"]}')
                    return (f"move_to_case_{best_resource_info['case']}", Priority.HIGH)
                
                priority_resource = missing_resources[0]
                return (f"search_{priority_resource}", Priority.HIGH)
        
        useful_resources = [stone for stone in STONES 
                          if self.game_state.inventory.get(stone, 0) < MAX_STUFF.get(stone, 1)]
        
        if useful_resources:
            best_resource_info = find_closest_resource(self.game_state.surroundings, useful_resources)
            if best_resource_info["found"]:
                self.logger.debug(f'� Found useful resource at case {best_resource_info["case"]}')
                return (f"move_to_case_{best_resource_info['case']}", Priority.MEDIUM)
        
        self.logger.debug('🚶 No specific target - exploring smartly')
        return ("smart_explore", Priority.LOW)

    
    def execute_action(self, action: Tuple[str, Priority]):
        """
        Enhanced action execution with coordination management
        """
        command, priority = action
        self.logger.info(f'⚡ Executing action: {command} (Priority: {priority.name})')

        if command.startswith("move_to_"):
            resource = command.split("_", 2)[2]
            self.turn_counter = move_to_resource(self.client, self.logger, self.game_state.surroundings, resource, self.turn_counter)

        elif command.startswith("search_"):
            resource = command.split("_", 1)[1]
            self.turn_counter = explore_for_resources(self.client, self.logger, resource, self.turn_counter)

        elif command.startswith("Take "):
            resource = command.split()[1]
            self.client.send_command(command)
            response = self.client.receive()
            if response == "ok":
                self.logger.info(f'✅ Successfully took {resource}')
                self.game_state.inventory[resource] = self.game_state.inventory.get(resource, 0) + 1
                if resource == "food":
                    self.game_state.food_units = self.game_state.inventory.get("food", 0)
            else:
                self.logger.warning(f'❌ Failed to take {resource}: {response}')

        elif command == "Incantation":
            self.logger.info('🔥 Starting elevation attempt!')
            result = attempt_elevation(self.client, self.game_state, self.logger)
            if result:
                self.logger.info(f'🎉 Elevation successful! New level: {self.game_state.level}')
            else:
                self.logger.error('💥 Elevation failed!')

        elif command in ["search_food", "search_food_aggressive", "Search for Food"]:
            if command == "search_food_aggressive":
                self.turn_counter = aggressive_food_search(self.client, self.logger, self.turn_counter)
            else:
                self.turn_counter = explore_for_resources(self.client, self.logger, "food", self.turn_counter)
            
        elif command == "fork":
            self.perform_fork()

        check_for_broadcasts(self.logger)

    def my_range(self, a: int, b: int):
        """Utility function to handle ranges in both directions"""
        if a < b:
            return range(a, b)
        return range(b, a)
    
    def need_object(self, obj: str) -> bool:
        """Check if the agent needs a specific object based on its inventory and maximum allowed quantities."""
        if obj == "":
            return False
        
        if obj == "food":
            return self.game_state.inventory.get("food", 0) < 20

        return self.game_state.inventory.get(obj, 0) < MAX_STUFF.get(obj, 1)
    
    def take_everything_on_tile(self):
        """Pick up everything useful on the current tile"""
        if not self.game_state.surroundings:
            return
            
        current_tile = self.game_state.surroundings[0]
        objects = current_tile.split() if current_tile else []
        
        for obj in objects:
            if obj == "player":
                continue
            if self.need_object(obj):
                self.logger.debug(f'💎 Taking {obj} from current tile')
                self.client.send_command(f"Take {obj}")
                response = self.client.receive()
                if response == "ok":
                    self.game_state.inventory[obj] = self.game_state.inventory.get(obj, 0) + 1
                    if obj == "food":
                        self.game_state.food_units = self.game_state.inventory.get("food", 0)
    
    def should_fork(self) -> bool:
        """Détermine si l'agent devrait tenter de fork"""
        
        if self.game_state.food_units < 20:
            self.logger.debug(f'🍴 Fork: Not enough food ({self.game_state.food_units} < 50)')
            return False
            
        if self.game_state.level <= 2:
            self.logger.debug(f'🍴 Fork: Level too low ({self.game_state.level} < 2)')
            return False

        self.client.send_command("Connect_nbr")
        connect_nbr_response = self.client.receive()
        try:
            available_slots = int(connect_nbr_response)
            if available_slots <= 0:
                self.logger.debug(f'🍴 Fork: No available slots ({available_slots})')
                return False
        except ValueError:
            self.logger.warning(f'🍴 Fork: Invalid connect_nbr response: {connect_nbr_response}')
            return False
            
        self.logger.info(f'🍴 Fork conditions met: food={self.game_state.food_units}, level={self.game_state.level}, slots={available_slots}')
        return True
    
    def perform_fork(self):
        """Exécute la commande fork"""
        self.logger.info('🍴 Executing fork command')
        self.client.send_command("Fork")
        response = self.client.receive()
        
        if response == "ok":
            self.logger.info('🎉 Fork successful! New player created')

            self.game_state.food_units = max(0, self.game_state.food_units - 42)
            self.game_state.inventory["food"] = self.game_state.food_units
        else:
            self.logger.error(f'❌ Fork failed: {response}')
    