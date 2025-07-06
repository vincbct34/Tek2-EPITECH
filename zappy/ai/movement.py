import random
from typing import List
from game_state import Pos

VISION_RANGES = [(0, 0), (1, 3), (4, 8), (9, 15), (16, 24), (25, 36), (37, 51), (52, 69), (70, 90)]

def my_range(a: int, b: int):
    """Utility function to handle ranges in both directions."""
    if a < b:
        return range(a, b)
    return range(b, a)

def go_to_case(client, logger, case_index: int):
    """Intelligent navigation to a specific case based on vision index."""
    if case_index == 0:
        return
        
    dest = Pos()
    level = 0
    
    for step in VISION_RANGES:
        if min(step) <= case_index <= max(step):
            dest.y = level
            dest.x = case_index - (max(step) - int((max(step) - min(step)) / 2))
            break
        level += 1
    
    logger.debug(f'🎯 Navigating to case {case_index}: dest({dest.x}, {dest.y})')
    
    if dest.x < 0:
        client.send_command("Left")
        client.receive()
    elif dest.x > 0:
        client.send_command("Right")
        client.receive()
        
    for i in my_range(0, abs(dest.x)):
        client.send_command("Forward")
        response = client.receive()
        if response != "ok":
            break
            
    if dest.x < 0:
        client.send_command("Right")
        client.receive()
    elif dest.x > 0:
        client.send_command("Left")
        client.receive()
        
    for i in my_range(0, dest.y):
        client.send_command("Forward")
        response = client.receive()
        if response != "ok":
            break

def smart_exploration(client, logger, turn_counter: int) -> int:
    """Optimized exploration movement with better coverage pattern."""
    turn_counter += 1
    
    # Pattern en spirale élargie pour couvrir plus de terrain
    if turn_counter % 10 == 0:
        # Tous les 10 tours, faire une grande exploration
        for _ in range(3):
            client.send_command("Forward")
            response = client.receive()
            if response != "ok":
                break
        client.send_command("Right")
        client.receive()
    elif turn_counter % 5 == 0:
        # Tous les 5 tours, changer de direction
        client.send_command("Left")
        client.receive()
        client.send_command("Forward")
        client.receive()
    else:
        # Mouvement normal : principalement avancer avec quelques rotations
        actions = ["Forward", "Forward", "Forward", "Left", "Right"]
        action = random.choice(actions)
        client.send_command(action)
        response = client.receive()
        
    return turn_counter

def move_to_resource(client, logger, surroundings: List[str], resource: str, turn_counter: int):
    """Intelligent movement strategy towards a resource."""
    from parsers import find_resource_in_view
    
    resource_index = find_resource_in_view(surroundings, resource)
    
    if resource_index == -1:
        return smart_exploration(client, logger, turn_counter)
    
    if resource_index == 0:
        return turn_counter
    
    go_to_case(client, logger, resource_index)
    return turn_counter

def explore_for_resources(client, logger, resource: str, turn_counter: int):
    """Intelligent exploration to find resources."""
    return smart_exploration(client, logger, turn_counter)