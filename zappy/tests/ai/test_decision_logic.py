import pytest
from unittest.mock import Mock, patch, MagicMock
from agent import Agent
from game_state import GameState
from constants import Priority, STONES, MAX_LEVEL, MAX_STUFF


class TestDecisionLogic:
    """Test suite specifically for the Agent's decision making logic"""
    
    @pytest.fixture
    def mock_client(self):
        """Create a mock client for testing"""
        client = Mock()
        client.get_world_size.return_value = (10, 10)
        client.team_name = "test_team"
        client.send_command = Mock()
        client.receive = Mock()
        return client
    
    @pytest.fixture
    def agent(self, mock_client):
        """Create an agent instance for testing"""
        with patch('agent.setup_logger') as mock_logger:
            mock_logger.return_value = Mock()
            return Agent(mock_client)
    
    def test_decide_action_critical_food_found(self, agent):
        """Test decision when food is critically low and food is visible"""
        agent.game_state.food_units = 15  # Below 20 threshold
        agent.game_state.surroundings = ["player", "food", ""]
        
        with patch.object(agent, 'take_everything_on_tile'), \
             patch.object(agent, 'find_closest_food') as mock_find_food:
            
            mock_find_food.return_value = {"found": True, "case": 1}
            
            action = agent.decide_action()
            
            assert action[0] == "move_to_case_1"
            assert action[1] == Priority.CRITICAL
    
    def test_decide_action_critical_food_not_found(self, agent):
        """Test decision when food is critically low and no food is visible"""
        agent.game_state.food_units = 15  # Below 20 threshold
        agent.game_state.surroundings = ["player", "linemate", ""]
        
        with patch.object(agent, 'take_everything_on_tile'), \
             patch.object(agent, 'find_closest_food') as mock_find_food:
            
            mock_find_food.return_value = {"found": False, "case": 0}
            
            action = agent.decide_action()
            
            assert action[0] == "search_food_aggressive"
            assert action[1] == Priority.CRITICAL
    
    def test_decide_action_preventive_food_collection(self, agent):
        """Test decision for preventive food collection when food is medium"""
        agent.game_state.food_units = 35  # Between 20 and 50
        agent.game_state.surroundings = ["player", "food", ""]
        
        with patch.object(agent, 'take_everything_on_tile'), \
             patch.object(agent, 'find_closest_food') as mock_find_food, \
             patch('agent.can_attempt_elevation') as mock_can_elevate:
            
            mock_find_food.return_value = {"found": True, "case": 1}
            mock_can_elevate.return_value = False
            
            action = agent.decide_action()
            
            assert action[0] == "move_to_case_1"
            assert action[1] == Priority.HIGH
    
    def test_decide_action_elevation_ready_enough_ready(self, agent):
        """Test decision when elevation is possible and enough players are ready"""
        agent.game_state.food_units = 60
        agent.game_state.is_ready = False
        
        with patch.object(agent, 'take_everything_on_tile'), \
             patch.object(agent, 'find_closest_food') as mock_find_food, \
             patch('agent.can_attempt_elevation') as mock_can_elevate, \
             patch('agent.enough_are_ready') as mock_enough_ready, \
             patch('agent.start_gathering') as mock_start_gathering:
            
            mock_find_food.return_value = {"found": False, "case": 0}
            mock_can_elevate.return_value = True
            mock_enough_ready.return_value = True
            
            action = agent.decide_action()
            
            assert action[0] == "gathering_started"
            assert action[1] == Priority.HIGH
            assert agent.game_state.is_ready == True
            mock_start_gathering.assert_called_once()
    
    def test_decide_action_elevation_ready_broadcast_ready(self, agent, mock_client):
        """Test decision when elevation is possible but need to broadcast ready"""
        agent.game_state.food_units = 60
        agent.game_state.is_ready = False
        
        with patch.object(agent, 'take_everything_on_tile'), \
             patch.object(agent, 'find_closest_food') as mock_find_food, \
             patch('agent.can_attempt_elevation') as mock_can_elevate, \
             patch('agent.enough_are_ready') as mock_enough_ready, \
             patch('agent.broadcast_message') as mock_broadcast:
            
            mock_find_food.return_value = {"found": False, "case": 0}
            mock_can_elevate.return_value = True
            mock_enough_ready.return_value = False
            
            action = agent.decide_action()
            
            assert action[0] == "wait_for_gathering"
            assert action[1] == Priority.HIGH
            assert agent.game_state.is_ready == True
            mock_broadcast.assert_called_once_with(
                mock_client, agent.logger, f"ready:{agent.game_state.level}"
            )
    
    def test_decide_action_fork_conditions_met(self, agent):
        """Test decision when fork conditions are met"""
        agent.game_state.food_units = 60
        
        with patch.object(agent, 'take_everything_on_tile'), \
             patch.object(agent, 'find_closest_food') as mock_find_food, \
             patch('agent.can_attempt_elevation') as mock_can_elevate, \
             patch.object(agent, 'should_fork') as mock_should_fork:
            
            mock_find_food.return_value = {"found": False, "case": 0}
            mock_can_elevate.return_value = False
            mock_should_fork.return_value = True
            
            action = agent.decide_action()
            
            assert action[0] == "fork"
            assert action[1] == Priority.HIGH
    
    def test_decide_action_need_resources_found(self, agent):
        """Test decision when agent needs resources for elevation and finds them"""
        agent.game_state.food_units = 60
        agent.game_state.level = 2  # Below MAX_LEVEL
        agent.game_state.surroundings = ["player", "linemate", ""]
        
        with patch.object(agent, 'take_everything_on_tile'), \
             patch.object(agent, 'find_closest_food') as mock_find_food, \
             patch('agent.can_attempt_elevation') as mock_can_elevate, \
             patch.object(agent, 'should_fork') as mock_should_fork, \
             patch('agent.get_needed_resources_for_elevation') as mock_get_needed, \
             patch.object(agent, 'find_closest_resource') as mock_find_resource:
            
            mock_find_food.return_value = {"found": False, "case": 0}
            mock_can_elevate.return_value = False
            mock_should_fork.return_value = False
            mock_get_needed.return_value = ["linemate", "deraumere"]
            mock_find_resource.return_value = {"found": True, "case": 1}
            
            action = agent.decide_action()
            
            assert action[0] == "move_to_case_1"
            assert action[1] == Priority.HIGH
    
    def test_decide_action_need_resources_not_found(self, agent):
        """Test decision when agent needs resources but can't find them"""
        agent.game_state.food_units = 60
        agent.game_state.level = 2  # Below MAX_LEVEL
        agent.game_state.surroundings = ["player", "food", ""]
        
        with patch.object(agent, 'take_everything_on_tile'), \
             patch.object(agent, 'find_closest_food') as mock_find_food, \
             patch('agent.can_attempt_elevation') as mock_can_elevate, \
             patch.object(agent, 'should_fork') as mock_should_fork, \
             patch('agent.get_needed_resources_for_elevation') as mock_get_needed, \
             patch.object(agent, 'find_closest_resource') as mock_find_resource:
            
            mock_find_food.return_value = {"found": False, "case": 0}
            mock_can_elevate.return_value = False
            mock_should_fork.return_value = False
            mock_get_needed.return_value = ["linemate", "deraumere"]
            mock_find_resource.return_value = {"found": False, "case": 0}
            
            action = agent.decide_action()
            
            assert action[0] == "search_linemate"  # First in the list
            assert action[1] == Priority.HIGH
    
    def test_decide_action_collect_useful_resources(self, agent):
        """Test decision when collecting useful resources below max capacity"""
        agent.game_state.food_units = 60
        agent.game_state.level = MAX_LEVEL  # At max level
        agent.game_state.inventory = {"linemate": 0}  # Below max
        agent.game_state.surroundings = ["player", "linemate", ""]
        
        with patch.object(agent, 'take_everything_on_tile'), \
             patch.object(agent, 'find_closest_food') as mock_find_food, \
             patch('agent.can_attempt_elevation') as mock_can_elevate, \
             patch.object(agent, 'should_fork') as mock_should_fork, \
             patch.object(agent, 'find_closest_resource') as mock_find_resource:
            
            mock_find_food.return_value = {"found": False, "case": 0}
            mock_can_elevate.return_value = False
            mock_should_fork.return_value = False
            mock_find_resource.return_value = {"found": True, "case": 1}
            
            action = agent.decide_action()
            
            assert action[0] == "move_to_case_1"
            assert action[1] == Priority.MEDIUM
    
    def test_decide_action_default_exploration(self, agent):
        """Test decision defaults to smart exploration when no specific action needed"""
        agent.game_state.food_units = 60
        agent.game_state.level = MAX_LEVEL  # At max level
        agent.game_state.inventory = {stone: MAX_STUFF.get(stone, 1) for stone in STONES}  # All at max
        agent.game_state.surroundings = ["player", "", ""]
        
        with patch.object(agent, 'take_everything_on_tile'), \
             patch.object(agent, 'find_closest_food') as mock_find_food, \
             patch('agent.can_attempt_elevation') as mock_can_elevate, \
             patch.object(agent, 'should_fork') as mock_should_fork, \
             patch.object(agent, 'find_closest_resource') as mock_find_resource:
            
            mock_find_food.return_value = {"found": False, "case": 0}
            mock_can_elevate.return_value = False
            mock_should_fork.return_value = False
            mock_find_resource.return_value = {"found": False, "case": 0}
            
            action = agent.decide_action()
            
            assert action[0] == "smart_explore"
            assert action[1] == Priority.LOW
    
    def test_decide_action_no_missing_resources(self, agent):
        """Test decision when no resources are missing for elevation"""
        agent.game_state.food_units = 60
        agent.game_state.level = 2  # Below MAX_LEVEL
        agent.game_state.surroundings = ["player", "linemate", ""]
        
        with patch.object(agent, 'take_everything_on_tile'), \
             patch.object(agent, 'find_closest_food') as mock_find_food, \
             patch('agent.can_attempt_elevation') as mock_can_elevate, \
             patch.object(agent, 'should_fork') as mock_should_fork, \
             patch('agent.get_needed_resources_for_elevation') as mock_get_needed, \
             patch.object(agent, 'find_closest_resource') as mock_find_resource:
            
            mock_find_food.return_value = {"found": False, "case": 0}
            mock_can_elevate.return_value = False
            mock_should_fork.return_value = False
            mock_get_needed.return_value = []  # No missing resources
            mock_find_resource.return_value = {"found": True, "case": 1}
            
            action = agent.decide_action()
            
            # Should look for useful resources
            assert action[0] == "move_to_case_1"
            assert action[1] == Priority.MEDIUM
    
    def test_decide_action_priority_ordering(self, agent):
        """Test that food takes priority over everything else when critically low"""
        agent.game_state.food_units = 10  # Critical
        agent.game_state.surroundings = ["player food linemate", "", ""]
        
        with patch.object(agent, 'take_everything_on_tile'), \
             patch.object(agent, 'find_closest_food') as mock_find_food, \
             patch('agent.can_attempt_elevation') as mock_can_elevate:
            
            mock_find_food.return_value = {"found": True, "case": 0}
            mock_can_elevate.return_value = True  # Even if elevation is ready
            
            action = agent.decide_action()
            
            # Food should take priority
            assert action[0] == "move_to_case_0"
            assert action[1] == Priority.CRITICAL