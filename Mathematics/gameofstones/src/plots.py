##
## EPITECH PROJECT, 2025
## Game of Stones
## File description:
## plots.py
##

from collections import deque
import itertools

from src.utilities import parse_friends_file, parse_conspiracies_file, build_adjacency_matrix

def floyd_warshall(matrix):

    """Optimized Floyd-Warshall algorithm to compute shortest paths.

    Args:
        matrix (list): Adjacency matrix representing the graph.

    Returns:
        list: Matrix of shortest paths between all pairs of nodes.
    """

    size = len(matrix)
    dist = [[float('inf')] * size for _ in range(size)]

    for i in range(size):
        dist[i][i] = 0
        for j in range(size):
            if matrix[i][j]:
                dist[i][j] = 1

    for k, i, j in itertools.product(range(size), repeat=3):
        dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j])

    return dist


def bfs_find_close_allies(start_person, index_map, adjacency_matrix, max_distance):

    """Use BFS to find all allies within max_distance from the start_person.

    Args:
        start_person (str): Name of the start person.
        index_map (dict): Dictionary mapping names to indices.
        adjacency_matrix (list): Adjacency matrix representing the graph.
        max_distance (int): Maximum distance to consider.

    Returns:
        set: Set of close allies.
    """

    close_allies = set()
    queue = deque([(start_person, 0)])
    visited = set()

    while queue:
        person, distance = queue.popleft()
        if distance > max_distance:
            continue

        if person != start_person:
            close_allies.add(person)

        visited.add(person)

        for neighbor, connected in enumerate(adjacency_matrix[index_map[person]]):
            if connected and neighbor not in visited:
                queue.append((list(index_map.keys())[neighbor], distance + 1))
                visited.add(neighbor)

    return close_allies


def bfs_best_conspirator(enemy, conspiracies, close_allies, distances, index_map):

    """Use BFS to find the best conspirator against a given enemy.

    Args:
        enemy (str): Name of the enemy.
        conspiracies (dict): Dictionary of conspiracies.
        close_allies (set): Set of close allies.
        distances (list): Floyd-Warshall distances.
        index_map (dict): Dictionary mapping names to indices.

    Returns:
        str: The name of the best conspirator.
    """

    queue = deque()
    visited = set()

    for ally in close_allies:
        if enemy in conspiracies.get(ally, []):
            queue.append((ally, distances[index_map["Cersei Lannister"]][index_map[ally]]))
            visited.add(ally)

    best_conspirator = None
    while queue:
        conspirator, distance = queue.popleft()

        if not best_conspirator or (
            conspirator not in conspiracies.get("Cersei Lannister", []) and best_conspirator in conspiracies.get("Cersei Lannister", [])
        ) or (
            distance < distances[index_map["Cersei Lannister"]][index_map[best_conspirator]]
        ) or (
            distance == distances[index_map["Cersei Lannister"]][index_map[best_conspirator]] and conspirator < best_conspirator
        ):
            best_conspirator = conspirator

    return best_conspirator


def eliminate_enemies_with_far_friends(remaining_enemies, conspiracies, close_allies, distances, index_map):

    return [], remaining_enemies


def eliminate_enemies_with_close_allies(enemies, conspiracies, close_allies, distances, index_map):

    """Optimized elimination of direct enemies using BFS to find best conspirators.

    Args:
        enemies (set): Set of enemies.
        conspiracies (dict): Dictionary of conspiracies.
        close_allies (set): Set of close allies.
        distances (list): Floyd-Warshall distances.
        index_map (dict): Dictionary mapping names to indices.

    Returns:
        tuple: A tuple containing the direct conspiracies and the remaining enemies.
    """

    direct_conspiracies = []
    remaining_enemies = set()

    for enemy in enemies:
        best_conspirator = bfs_best_conspirator(enemy, conspiracies, close_allies, distances, index_map)

        if best_conspirator:
            direct_conspiracies.append([best_conspirator, enemy])
        else:
            remaining_enemies.add(enemy)

    direct_conspiracies.sort()
    return direct_conspiracies, remaining_enemies


def analyze_conspiracy_plots(friends_filepath, conspiracies_filepath, n):

    """Analyze plots and determine if the Queen is saved.

    Args:
        friends_filepath (str): Path to the file containing friendships.
        conspiracies_filepath (str): Path to the file containing conspiracies.
        n (int): Maximum path length to consider.

    Returns:
        tuple: A tuple containing the names of the characters, the relationships between them, the conspiracies, and a boolean indicating if the Queen is saved.
    """

    friends_graph = parse_friends_file(friends_filepath)
    plots_graph = parse_conspiracies_file(conspiracies_filepath, friends_graph)

    names = sorted(friends_graph.keys(), key=lambda x: (x != "Cersei Lannister", x))
    matrix, index_map = build_adjacency_matrix(friends_graph)

    distances = floyd_warshall(matrix)
    relationships = [[dist if dist <= n else float('inf') for dist in row] for row in distances]

    close_allies = bfs_find_close_allies("Cersei Lannister", index_map, matrix, n)
    enemies = {enemy for enemy, targets in plots_graph.items() if "Cersei Lannister" in targets}

    direct_conspiracies, remaining_enemies = eliminate_enemies_with_close_allies(enemies, plots_graph, close_allies, distances, index_map)
    indirect_conspiracies, still_remaining = eliminate_enemies_with_far_friends(remaining_enemies, plots_graph, close_allies, distances, index_map)

    result = len(still_remaining) == 0

    return names, relationships, direct_conspiracies + indirect_conspiracies, result, still_remaining if not result else None
