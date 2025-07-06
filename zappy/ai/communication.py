def broadcast_message(client, logger, message: str):
    """Broadcast a message to the team with improved handling."""
    logger.debug(f'📢 Broadcasting: {message}')
    client.send_command(f"Broadcast {message}")
    response = client.receive()
    return response

def check_for_broadcasts(logger):
    """Check if there are broadcast messages pending."""
    try:
        pass
    except Exception as e:
        logger.debug(f'📡 No broadcast messages: {e}')

def handle_ready_msg(game_state, logger, msg: str) -> str:
    """Handle 'ready' messages from other players."""
    try:
        level = int(msg.split(":")[1])
        game_state.dispo_members[level] += 1
        logger.debug(f'📢 Player ready for level {level}. Total ready: {game_state.dispo_members[level]}')
        return "continue"
    except (IndexError, ValueError):
        logger.warning(f'⚠️ Invalid ready message format: {msg}')
        return "continue"

def handle_start_msg(game_state, logger, msg: str) -> str:
    """Handle 'start' messages for gatherings."""
    try:
        parts = msg.split(":")[1].split("-")
        level = int(parts[0])
        zone_id = int(parts[1])
        
        from elevation import ELEVATION_REQUIREMENTS
        if not game_state.is_ready:
            needed = ELEVATION_REQUIREMENTS[level]["players"]
            game_state.dispo_members[level] -= (needed - 1)
        
        if (not game_state.is_joining and 
            game_state.is_ready and 
            level == game_state.level):
            
            logger.info(f'🎯 Joining gathering zone {zone_id} for level {level}')
            game_state.zone_id = zone_id
            game_state.is_joining = True
            
        return "quit"
    except (IndexError, ValueError):
        logger.warning(f'⚠️ Invalid start message format: {msg}')
        return "continue"

def handle_zone_msg(game_state, logger, msg: str) -> str:
    """Handle 'zone' messages to join a zone."""
    try:
        zone_id = int(msg.split(":")[1])
        
        if game_state.is_joining and game_state.zone_id == zone_id:
            logger.info(f'🏃 Moving to join zone {zone_id}')
            join_zone(game_state, logger, msg)
            return "quit"
            
        return "continue"
    except (IndexError, ValueError):
        logger.warning(f'⚠️ Invalid zone message format: {msg}')
        return "continue"

def join_zone(client, game_state, logger, broadcast_msg: str):
    """Join a zone based on a broadcast message."""
    try:
        parts = broadcast_msg.split(",")
        if len(parts) < 2:
            return
            
        orientation = int(parts[0].split(" ")[1])
        logger.debug(f'🧭 Zone orientation: {orientation}')
        
        from elevation import start_incantation_process
        
        if orientation == 0:
            start_incantation_process(client, game_state, logger, active=False)
            return
        elif orientation == 1:
            client.send_command("Forward")
            client.receive()
        elif orientation in [2, 3, 4]:
            client.send_command("Left")
            client.receive()
            client.send_command("Forward")
            client.receive()
        elif orientation in [6, 7, 8]:
            client.send_command("Right")
            client.receive()
            client.send_command("Forward")
            client.receive()
        elif orientation == 5:
            client.send_command("Left")
            client.receive()
            client.send_command("Left")
            client.receive()
            
    except (IndexError, ValueError):
        logger.warning(f'⚠️ Invalid broadcast message for zone joining: {broadcast_msg}')