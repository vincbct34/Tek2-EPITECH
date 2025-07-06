import re
import random
from typing import Dict, List
from constants import ELEVATION_REQUIREMENTS

def get_needed_resources_for_elevation(game_state) -> List[str]:
    """Get the list of resources needed for the next elevation."""
    current_level = game_state.level
    if current_level >= 8:
        return []
    requirements = ELEVATION_REQUIREMENTS[current_level]
    needed_resources = []

    for resource, quantity in requirements.items():
        if resource == "players":
            continue

        current_quantity = game_state.inventory.get(resource, 0)
        if current_quantity < quantity:
            needed_resources.extend([resource] * (quantity - current_quantity))
    return needed_resources

def can_attempt_elevation(game_state, logger) -> bool:
    """Check if the agent can attempt elevation with player detection."""
    current_level = game_state.level
    if current_level >= 8:
        logger.debug('🏆 Already at max level')
        return False
    
    requirements = ELEVATION_REQUIREMENTS[current_level]
    logger.debug(f'🔍 Checking elevation requirements for level {current_level}: {requirements}')
    
    needed_players = requirements.get("players", 1)
    from parsers import count_players_on_current_tile
    current_players = count_players_on_current_tile(game_state.surroundings)
    
    if current_players < needed_players:
        logger.debug(f'👥 Not enough players: {current_players}/{needed_players}')
        return False
    
    missing_resources = []
    for resource, quantity in requirements.items():
        if resource == "players":
            continue
        
        current_quantity = game_state.inventory.get(resource, 0)
        if current_quantity < quantity:
            missing_resources.append(f"{resource}: {current_quantity}/{quantity}")
        else:
            logger.debug(f'✅ {resource}: {current_quantity}/{quantity}')

    if missing_resources:
        logger.debug(f'❌ Missing resources: {missing_resources}')
        return False
    
    logger.info('🚀 All elevation requirements met!')
    return True

def attempt_elevation(client, game_state, logger):
    """Attempt the elevation ritual."""
    logger.info('🔮 Preparing elevation ritual...')
    
    current_level = game_state.level
    requirements = ELEVATION_REQUIREMENTS[current_level]
    
    for resource, quantity in requirements.items():
        if resource == "players":
            continue
            
        if game_state.inventory.get(resource, 0) >= quantity:
            logger.debug(f'💎 Placing {quantity}x {resource} on ground')
            for _ in range(quantity):
                client.send_command(f"Set {resource}")
                response = client.receive()
                if response != "ok":
                    logger.warning(f'⚠️ Failed to set {resource}: {response}')
    
    logger.info('✨ Starting incantation...')
    client.send_command("Incantation")
    response = client.receive()
    
    if "Elevation underway" in response:
        logger.info('🔥 Elevation ritual started! Waiting for completion...')
        final_response = client.receive()
        logger.info(f'📝 Final elevation response: {final_response}')
        match = re.search(r"Current level: (\d+)", final_response)
        if match:
            new_level = int(match.group(1))
            old_level = game_state.level
            game_state.level = new_level
            logger.info(f'🎊 LEVEL UP! {old_level} → {new_level}')
            return True
        else:
            return True
    else:
        logger.error(f'💥 Elevation failed: {response}')
        return False

def enough_are_ready(game_state) -> bool:
    """Check if enough players are ready for incantation."""
    needed = ELEVATION_REQUIREMENTS[game_state.level]["players"]
    ready = game_state.dispo_members[game_state.level]
    return ready >= (needed - 1)

def start_gathering(client, game_state, logger):
    """Start the gathering process for incantation."""
    logger.info('🚀 Starting gathering process')
    
    if game_state.level == 1:
        start_incantation_process(client, game_state, logger, active=True)
        return
        
    game_state.zone_id = random.randint(1000, 3000)
    logger.info(f'📍 Broadcasting gathering zone: {game_state.zone_id}')
    
    from communication import broadcast_message
    broadcast_message(client, logger, f"start:{game_state.level}-{game_state.zone_id}")
    
    wait_count = 0
    max_wait = 20
    
    while not_enough_players(client, game_state, logger) and wait_count < max_wait:
        broadcast_message(client, logger, f"zone:{game_state.zone_id}")
        client.send_command("Left")
        client.receive()
        wait_count += 1
        
    game_state.zone_id = 0
    
    if wait_count < max_wait:
        start_incantation_process(client, game_state, logger, active=True)
    else:
        logger.warning('⏰ Gathering timeout - aborting')

def not_enough_players(client, game_state, logger) -> bool:
    """Check if there are enough players on the current tile."""
    needed = ELEVATION_REQUIREMENTS[game_state.level]["players"]
    from parsers import count_players_on_current_tile
    current_players = count_players_on_current_tile(game_state.surroundings)
    
    if current_players < needed:
        return True
        
    if game_state.zone_id:
        from communication import broadcast_message
        broadcast_message(client, logger, f"zone:{game_state.zone_id}")
        
    return False

def start_incantation_process(client, game_state, logger, active: bool = True):
    """Start the incantation process."""
    logger.info(f'✨ Starting incantation ({"active" if active else "passive"})')
    
    requirements = ELEVATION_REQUIREMENTS[game_state.level]
    for stone, quantity in requirements.items():
        if stone == "players":
            continue
            
        for _ in range(quantity):
            if game_state.inventory.get(stone, 0) > 0:
                client.send_command(f"Set {stone}")
                response = client.receive()
                if response == "ok":
                    game_state.inventory[stone] -= 1
    
    if active:
        success = attempt_elevation(client, game_state, logger)
        if success:
            game_state.level += 1
            logger.info(f'🎉 Level up! Now level {game_state.level}')
        
    game_state.is_ready = False
    game_state.is_joining = False