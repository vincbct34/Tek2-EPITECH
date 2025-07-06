import pytest
from game_state import GameState, Pos, MAX_LEVEL


class TestPos:
    """Test suite for the Pos class"""
    
    def test_pos_default_initialization(self):
        """Test Pos initialization with default values"""
        pos = Pos()
        assert pos.x == 0
        assert pos.y == 0
    
    def test_pos_custom_initialization(self):
        """Test Pos initialization with custom values"""
        pos = Pos(5, 10)
        assert pos.x == 5
        assert pos.y == 10
    
    def test_pos_negative_values(self):
        """Test Pos with negative coordinates"""
        pos = Pos(-3, -7)
        assert pos.x == -3
        assert pos.y == -7


class TestGameState:
    """Test suite for the GameState class"""
    
    def test_game_state_initialization(self):
        """Test GameState initialization with required parameters"""
        game_state = GameState(
            inventory={"food": 10, "linemate": 2},
            position=(5, 5),
            level=2,
            food_units=10,
            world_size=(20, 20),
            team_name="test_team",
            surroundings=["player", "food", "linemate"]
        )
        
        assert game_state.inventory == {"food": 10, "linemate": 2}
        assert game_state.position == (5, 5)
        assert game_state.level == 2
        assert game_state.food_units == 10
        assert game_state.world_size == (20, 20)
        assert game_state.team_name == "test_team"
        assert game_state.surroundings == ["player", "food", "linemate"]
    
    def test_game_state_default_values(self):
        """Test GameState initialization with default optional values"""
        game_state = GameState(
            inventory={},
            position=None,
            level=1,
            food_units=0,
            world_size=(10, 10),
            team_name="default_team",
            surroundings=[]
        )
        
        # Test default values
        assert game_state.zone_id == 0
        assert game_state.is_ready == False
        assert game_state.is_joining == False
        assert game_state.last_look == ""
        assert game_state.dispo_members == [0] * (MAX_LEVEL + 1)
    
    def test_game_state_custom_optional_values(self):
        """Test GameState initialization with custom optional values"""
        custom_dispo = [1, 2, 3, 4, 5, 6, 7, 8, 9]
        game_state = GameState(
            inventory={"food": 5},
            position=(0, 0),
            level=3,
            food_units=5,
            world_size=(15, 15),
            team_name="custom_team",
            surroundings=["player"],
            zone_id=5,
            is_ready=True,
            is_joining=True,
            dispo_members=custom_dispo,
            last_look="[player], [food], []"
        )
        
        assert game_state.zone_id == 5
        assert game_state.is_ready == True
        assert game_state.is_joining == True
        assert game_state.dispo_members == custom_dispo
        assert game_state.last_look == "[player], [food], []"
    
    def test_game_state_post_init_default_dispo_members(self):
        """Test that __post_init__ correctly sets default dispo_members"""
        game_state = GameState(
            inventory={},
            position=None,
            level=1,
            food_units=0,
            world_size=(10, 10),
            team_name="test",
            surroundings=[]
        )
        
        # Should create default dispo_members with correct length
        expected_length = MAX_LEVEL + 1
        assert len(game_state.dispo_members) == expected_length
        assert all(member == 0 for member in game_state.dispo_members)
    
    def test_game_state_post_init_preserves_custom_dispo_members(self):
        """Test that __post_init__ preserves custom dispo_members"""
        custom_dispo = [1, 2, 3, 4, 5]
        game_state = GameState(
            inventory={},
            position=None,
            level=1,
            food_units=0,
            world_size=(10, 10),
            team_name="test",
            surroundings=[],
            dispo_members=custom_dispo
        )
        
        # Should preserve the custom dispo_members
        assert game_state.dispo_members == custom_dispo
    
    def test_game_state_inventory_modification(self):
        """Test that inventory can be modified after initialization"""
        game_state = GameState(
            inventory={"food": 10},
            position=None,
            level=1,
            food_units=10,
            world_size=(10, 10),
            team_name="test",
            surroundings=[]
        )
        
        # Modify inventory
        game_state.inventory["linemate"] = 5
        game_state.inventory["food"] = 15
        
        assert game_state.inventory["linemate"] == 5
        assert game_state.inventory["food"] == 15
    
    def test_game_state_surroundings_modification(self):
        """Test that surroundings can be modified after initialization"""
        game_state = GameState(
            inventory={},
            position=None,
            level=1,
            food_units=0,
            world_size=(10, 10),
            team_name="test",
            surroundings=["player"]
        )
        
        # Modify surroundings
        game_state.surroundings.append("food")
        game_state.surroundings.append("linemate")
        
        assert len(game_state.surroundings) == 3
        assert "food" in game_state.surroundings
        assert "linemate" in game_state.surroundings
    
    def test_game_state_level_bounds(self):
        """Test GameState with different level values"""
        # Test minimum level
        game_state_min = GameState(
            inventory={},
            position=None,
            level=1,
            food_units=0,
            world_size=(10, 10),
            team_name="test",
            surroundings=[]
        )
        assert game_state_min.level == 1
        
        # Test maximum level
        game_state_max = GameState(
            inventory={},
            position=None,
            level=MAX_LEVEL,
            food_units=0,
            world_size=(10, 10),
            team_name="test",
            surroundings=[]
        )
        assert game_state_max.level == MAX_LEVEL
    
    def test_game_state_world_size_variations(self):
        """Test GameState with different world sizes"""
        world_sizes = [(10, 10), (5, 15), (20, 20), (1, 1), (100, 50)]
        
        for world_size in world_sizes:
            game_state = GameState(
                inventory={},
                position=None,
                level=1,
                food_units=0,
                world_size=world_size,
                team_name="test",
                surroundings=[]
            )
            assert game_state.world_size == world_size
    
    def test_game_state_position_none(self):
        """Test GameState with None position"""
        game_state = GameState(
            inventory={},
            position=None,
            level=1,
            food_units=0,
            world_size=(10, 10),
            team_name="test",
            surroundings=[]
        )
        
        assert game_state.position is None
    
    def test_game_state_empty_collections(self):
        """Test GameState with empty collections"""
        game_state = GameState(
            inventory={},
            position=None,
            level=1,
            food_units=0,
            world_size=(10, 10),
            team_name="",
            surroundings=[]
        )
        
        assert len(game_state.inventory) == 0
        assert len(game_state.surroundings) == 0
        assert game_state.team_name == ""