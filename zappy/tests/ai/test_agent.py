import pytest
from unittest.mock import Mock, patch, MagicMock
from agent import Agent
from game_state import GameState
from constants import Priority, STONES, MAX_LEVEL, MAX_STUFF, ELEVATION_REQUIREMENTS


class TestAgent:
    """Test suite for the Agent class"""
    
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
    
    def test_agent_initialization(self, mock_client):
        """Test agent initialization"""
        with patch('agent.setup_logger') as mock_logger:
            mock_logger.return_value = Mock()
            agent = Agent(mock_client)
            
            assert agent.client == mock_client
            assert agent.game_state.team_name == "test_team"
            assert agent.game_state.level == 1
            assert agent.game_state.food_units == 10
            assert agent.game_state.world_size == (10, 10)
            assert agent.turn_counter == 0
    
    def test_update_game_state_valid_responses(self, agent, mock_client):
        """Test update_game_state with valid responses"""
        # Mock valid responses
        mock_client.receive.side_effect = [
            "[player, linemate food], [food], []",  # Look response
            "food 5, linemate 2"  # Inventory response
        ]
        
        with patch('agent.parse_look_response') as mock_parse_look, \
             patch('agent.parse_inventory_response') as mock_parse_inv:
            
            mock_parse_look.return_value = ["player linemate food", "food", ""]
            mock_parse_inv.return_value = {"food": 5, "linemate": 2}
            
            agent.update_game_state()
            
            # Verify commands were sent
            assert mock_client.send_command.call_count == 2
            mock_client.send_command.assert_any_call("Look")
            mock_client.send_command.assert_any_call("Inventory")
            
            # Verify state updates
            assert agent.game_state.surroundings == ["player linemate food", "food", ""]
            assert agent.game_state.inventory == {"food": 5, "linemate": 2}
            assert agent.game_state.food_units == 5
    
    def test_update_game_state_invalid_look(self, agent, mock_client):
        """Test update_game_state with invalid Look response"""
        mock_client.receive.side_effect = [
            "ko",  # Invalid Look response
        ]
        
        agent.update_game_state()
        
        # Should only send Look command, then return early
        assert mock_client.send_command.call_count == 1
        mock_client.send_command.assert_called_with("Look")
    
    def test_need_object_food(self, agent):
        """Test need_object function for food"""
        agent.game_state.inventory = {"food": 15}
        assert agent.need_object("food") == True  # Below 20 threshold
        
        agent.game_state.inventory = {"food": 25}
        assert agent.need_object("food") == False  # Above 20 threshold
    
    def test_need_object_stones(self, agent):
        """Test need_object function for stones"""
        agent.game_state.inventory = {"linemate": 0}
        assert agent.need_object("linemate") == True
        
        agent.game_state.inventory = {"linemate": MAX_STUFF.get("linemate", 1)}
        assert agent.need_object("linemate") == False
    
    def test_need_object_empty_string(self, agent):
        """Test need_object with empty string"""
        assert agent.need_object("") == False
    
    def test_take_everything_on_tile(self, agent, mock_client):
        """Test take_everything_on_tile function"""
        agent.game_state.surroundings = ["player food linemate"]
        agent.game_state.inventory = {"food": 5, "linemate": 0}
        
        mock_client.receive.side_effect = ["ok", "ok"]  # Both takes succeed
        
        agent.take_everything_on_tile()
        
        # Should attempt to take food and linemate but not player
        assert mock_client.send_command.call_count == 2
        mock_client.send_command.assert_any_call("Take food")
        mock_client.send_command.assert_any_call("Take linemate")
        
        # Inventory should be updated
        assert agent.game_state.inventory["food"] == 6
        assert agent.game_state.inventory["linemate"] == 1
    
    def test_should_fork_insufficient_food(self, agent, mock_client):
        """Test should_fork with insufficient food"""
        agent.game_state.food_units = 10  # Below threshold
        agent.game_state.level = 3
        
        assert agent.should_fork() == False
    
    def test_should_fork_low_level(self, agent, mock_client):
        """Test should_fork with low level"""
        agent.game_state.food_units = 50
        agent.game_state.level = 1  # Below threshold
        
        assert agent.should_fork() == False
    
    def test_should_fork_no_slots(self, agent, mock_client):
        """Test should_fork with no available slots"""
        agent.game_state.food_units = 50
        agent.game_state.level = 3
        mock_client.receive.return_value = "0"  # No slots
        
        assert agent.should_fork() == False
        mock_client.send_command.assert_called_with("Connect_nbr")
    
    def test_should_fork_success(self, agent, mock_client):
        """Test should_fork with all conditions met"""
        agent.game_state.food_units = 50
        agent.game_state.level = 3
        mock_client.receive.return_value = "5"  # Available slots
        
        assert agent.should_fork() == True
    
    def test_perform_fork_success(self, agent, mock_client):
        """Test successful fork execution"""
        agent.game_state.food_units = 50
        agent.game_state.inventory = {"food": 50}
        mock_client.receive.return_value = "ok"
        
        agent.perform_fork()
        
        mock_client.send_command.assert_called_with("Fork")
        assert agent.game_state.food_units == 8  # 50 - 42
        assert agent.game_state.inventory["food"] == 8
    
    def test_perform_fork_failure(self, agent, mock_client):
        """Test failed fork execution"""
        initial_food = 50
        agent.game_state.food_units = initial_food
        agent.game_state.inventory = {"food": initial_food}
        mock_client.receive.return_value = "ko"
        
        agent.perform_fork()
        
        # Food should remain unchanged on failure
        assert agent.game_state.food_units == initial_food
    
    def test_find_closest_food_found(self, agent):
        """Test find_closest_food when food is present"""
        agent.game_state.surroundings = ["player", "food linemate", ""]
        
        result = agent.find_closest_food()
        
        assert result["found"] == True
        assert result["case"] == 1
    
    def test_find_closest_food_not_found(self, agent):
        """Test find_closest_food when no food is present"""
        agent.game_state.surroundings = ["player", "linemate", ""]
        
        result = agent.find_closest_food()
        
        assert result["found"] == False
        assert result["case"] == 0
    
    def test_find_closest_resource_found(self, agent):
        """Test find_closest_resource when resource is present"""
        agent.game_state.surroundings = ["player", "deraumere", "linemate sibur"]
        resources = ["linemate", "sibur"]
        
        result = agent.find_closest_resource(resources)
        
        assert result["found"] == True
        assert result["case"] == 2  # First case with any of the resources
    
    def test_find_closest_resource_not_found(self, agent):
        """Test find_closest_resource when no resource is present"""
        agent.game_state.surroundings = ["player", "food", ""]
        resources = ["linemate", "sibur"]
        
        result = agent.find_closest_resource(resources)
        
        assert result["found"] == False
        assert result["case"] == 0
    
    @patch('agent.explore_for_resources')
    def test_aggressive_food_search(self, mock_explore, agent, mock_client):
        """Test aggressive_food_search function"""
        mock_client.receive.side_effect = ["ok"] * 9 + ["[food], [], []"] * 9
        
        with patch('agent.parse_look_response') as mock_parse, \
             patch('agent.have_seen_stones') as mock_seen, \
             patch('agent.go_to_case') as mock_go:
            
            mock_parse.return_value = ["food", "", ""]
            mock_seen.return_value = {"found": True, "case": 0}
            
            result = agent.aggressive_food_search()
            
            # Should have moved in multiple directions
            assert mock_client.send_command.call_count >= 2
            mock_go.assert_called_once()
    
    def test_execute_action_move_to_case(self, agent, mock_client):
        """Test execute_action with move_to_case command"""
        with patch('agent.go_to_case') as mock_go:
            agent.execute_action(("move_to_case_5", Priority.HIGH))
            mock_go.assert_called_with(mock_client, agent.logger, 5)
    
    def test_execute_action_take(self, agent, mock_client):
        """Test execute_action with Take command"""
        mock_client.receive.return_value = "ok"
        agent.game_state.inventory = {"food": 5}
        
        agent.execute_action(("Take food", Priority.HIGH))
        
        mock_client.send_command.assert_called_with("Take food")
        assert agent.game_state.inventory["food"] == 6
    
    def test_execute_action_search(self, agent, mock_client):
        """Test execute_action with search command"""
        with patch('agent.explore_for_resources') as mock_explore:
            mock_explore.return_value = 5
            
            agent.execute_action(("search_food", Priority.HIGH))
            
            mock_explore.assert_called_with(mock_client, agent.logger, "food", 0)
            assert agent.turn_counter == 5
    
    def test_execute_action_fork(self, agent, mock_client):
        """Test execute_action with fork command"""
        agent.game_state.food_units = 50
        mock_client.receive.return_value = "ok"
        
        agent.execute_action(("fork", Priority.HIGH))
        
        mock_client.send_command.assert_called_with("Fork")