import pytest
from parsers import (
    parse_look_response, 
    parse_inventory_response, 
    find_resource_in_view,
    count_players_on_current_tile,
    have_seen_stones
)


class TestParsers:
    """Test suite for parser functions"""
    
    def test_parse_look_response_simple(self):
        """Test parsing a simple look response"""
        response = "[player, food, linemate]"
        result = parse_look_response(response)
        
        expected = ["player", "food", "linemate"]
        assert result == expected
    
    def test_parse_look_response_complex(self):
        """Test parsing a complex look response with multiple items per tile"""
        response = "[player food, linemate deraumere, , food]"
        result = parse_look_response(response)
        
        expected = ["player food", "linemate deraumere", "", "food"]
        assert result == expected
    
    def test_parse_look_response_empty_tiles(self):
        """Test parsing look response with empty tiles"""
        response = "[player, , , food, ]"
        result = parse_look_response(response)
        
        expected = ["player", "", "", "food", ""]
        assert result == expected
    
    def test_parse_look_response_single_tile(self):
        """Test parsing look response with single tile"""
        response = "[player food linemate]"
        result = parse_look_response(response)
        
        expected = ["player food linemate"]
        assert result == expected
    
    def test_parse_inventory_response_valid(self):
        """Test parsing a valid inventory response"""
        response = "[food 10, linemate 3, deraumere 1]"
        result = parse_inventory_response(response)
        
        expected = {"food": 10, "linemate": 3, "deraumere": 1}
        assert result == expected
    
    def test_parse_inventory_response_single_item(self):
        """Test parsing inventory response with single item"""
        response = "[food 5]"
        result = parse_inventory_response(response)
        
        expected = {"food": 5}
        assert result == expected
    
    def test_parse_inventory_response_empty(self):
        """Test parsing empty inventory response"""
        response = "[]"
        result = parse_inventory_response(response)
        
        expected = {}
        assert result == expected
    
    def test_parse_inventory_response_invalid_format(self):
        """Test parsing inventory response with invalid format"""
        response = "invalid response"
        result = parse_inventory_response(response)
        
        expected = {}
        assert result == expected
    
    def test_parse_inventory_response_no_brackets(self):
        """Test parsing inventory response without brackets"""
        response = "food 10, linemate 3"
        result = parse_inventory_response(response)
        
        expected = {}
        assert result == expected
    
    def test_find_resource_in_view_found_first_tile(self):
        """Test finding resource in first tile"""
        surroundings = ["player food", "linemate", ""]
        result = find_resource_in_view(surroundings, "food")
        
        assert result == 0
    
    def test_find_resource_in_view_found_middle_tile(self):
        """Test finding resource in middle tile"""
        surroundings = ["player", "linemate deraumere", "food"]
        result = find_resource_in_view(surroundings, "deraumere")
        
        assert result == 1
    
    def test_find_resource_in_view_found_last_tile(self):
        """Test finding resource in last tile"""
        surroundings = ["player", "linemate", "food sibur"]
        result = find_resource_in_view(surroundings, "sibur")
        
        assert result == 2
    
    def test_find_resource_in_view_not_found(self):
        """Test finding resource that doesn't exist"""
        surroundings = ["player", "linemate", "food"]
        result = find_resource_in_view(surroundings, "deraumere")
        
        assert result == -1
    
    def test_find_resource_in_view_empty_surroundings(self):
        """Test finding resource in empty surroundings"""
        surroundings = []
        result = find_resource_in_view(surroundings, "food")
        
        assert result == -1
    
    def test_find_resource_in_view_empty_tiles(self):
        """Test finding resource in empty tiles"""
        surroundings = ["", "", ""]
        result = find_resource_in_view(surroundings, "food")
        
        assert result == -1
    
    def test_count_players_on_current_tile_multiple_players(self):
        """Test counting multiple players on current tile"""
        surroundings = ["player player player", "food", "linemate"]
        result = count_players_on_current_tile(surroundings)
        
        assert result == 3
    
    def test_count_players_on_current_tile_single_player(self):
        """Test counting single player on current tile"""
        surroundings = ["player food", "linemate", ""]
        result = count_players_on_current_tile(surroundings)
        
        assert result == 1
    
    def test_count_players_on_current_tile_no_players(self):
        """Test counting when no players on current tile"""
        surroundings = ["food linemate", "deraumere", ""]
        result = count_players_on_current_tile(surroundings)
        
        assert result == 0
    
    def test_count_players_on_current_tile_empty_surroundings(self):
        """Test counting players with empty surroundings"""
        surroundings = []
        result = count_players_on_current_tile(surroundings)
        
        assert result == 0
    
    def test_count_players_on_current_tile_empty_first_tile(self):
        """Test counting players with empty first tile"""
        surroundings = ["", "player", "food"]
        result = count_players_on_current_tile(surroundings)
        
        assert result == 0
    
    def test_have_seen_stones_food_priority(self):
        """Test that food takes priority in have_seen_stones"""
        surroundings = ["player", "linemate", "food deraumere"]
        missing_stones = ["linemate", "food", "deraumere"]
        result = have_seen_stones(surroundings, missing_stones)
        
        assert result["found"] == True
        assert result["case"] == 2
        assert result["resource"] == "food"
    
    def test_have_seen_stones_non_food_resource(self):
        """Test finding non-food resource when food not in list"""
        surroundings = ["player", "linemate sibur", "deraumere"]
        missing_stones = ["linemate", "deraumere"]
        result = have_seen_stones(surroundings, missing_stones)
        
        assert result["found"] == True
        assert result["case"] == 1  # First occurrence
        assert result["resource"] == "linemate"
    
    def test_have_seen_stones_closest_first(self):
        """Test that closest resource is returned first"""
        surroundings = ["player", "deraumere", "linemate"]
        missing_stones = ["linemate", "deraumere"]
        result = have_seen_stones(surroundings, missing_stones)
        
        assert result["found"] == True
        assert result["case"] == 1  # Closest deraumere
        assert result["resource"] == "deraumere"
    
    def test_have_seen_stones_not_found(self):
        """Test when no stones are found"""
        surroundings = ["player", "food", ""]
        missing_stones = ["linemate", "deraumere"]
        result = have_seen_stones(surroundings, missing_stones)
        
        assert result["found"] == False
        assert result["case"] == 0
        assert result["resource"] == ""
    
    def test_have_seen_stones_empty_surroundings(self):
        """Test with empty surroundings"""
        surroundings = []
        missing_stones = ["food", "linemate"]
        result = have_seen_stones(surroundings, missing_stones)
        
        assert result["found"] == False
        assert result["case"] == 0
        assert result["resource"] == ""
    
    def test_have_seen_stones_empty_missing_stones(self):
        """Test with empty missing stones list"""
        surroundings = ["player food", "linemate", ""]
        missing_stones = []
        result = have_seen_stones(surroundings, missing_stones)
        
        assert result["found"] == False
        assert result["case"] == 0
        assert result["resource"] == ""
    
    def test_have_seen_stones_food_only(self):
        """Test when only food is in missing stones"""
        surroundings = ["player", "linemate", "food"]
        missing_stones = ["food"]
        result = have_seen_stones(surroundings, missing_stones)
        
        assert result["found"] == True
        assert result["case"] == 2
        assert result["resource"] == "food"
    
    def test_have_seen_stones_multiple_same_resource(self):
        """Test with multiple instances of same resource"""
        surroundings = ["player linemate", "linemate", "food linemate"]
        missing_stones = ["linemate"]
        result = have_seen_stones(surroundings, missing_stones)
        
        assert result["found"] == True
        assert result["case"] == 0  # First occurrence
        assert result["resource"] == "linemate"