##
## EPITECH PROJECT, 2025
## Game of Stones
## File description:
## links.py
##

from collections import deque

from src.utilities import parse_friends_file, build_adjacency_matrix


def degree_of_separation(matrix, node_index, person1, person2):

    """Optimized BFS for finding degree of separation.

    Args:
        matrix (list): Adjacency matrix representing the graph.
        node_index (dict): Dictionary mapping person names to their index in the matrix.
        person1 (str): Name of the first person.
        person2 (str): Name of the second person.

    Returns:
        int: Degree of separation between person1 and person2, or -1 if they are not connected.
    """

    if person1 not in node_index or person2 not in node_index:
        return -1

    start, end = node_index[person1], node_index[person2]
    visited = set()
    queue = deque([(start, 0)])

    while queue:
        current, degree = queue.popleft()
        if current == end:
            return degree

        for neighbor, connected in enumerate(matrix[current]):
            if connected and neighbor not in visited:
                visited.add(neighbor)
                queue.append((neighbor, degree + 1))

    return -1


def get_degree_of_separation_from_file(filepath, person1, person2):

    """Get the degree of separation between two people from a file.

    Args:
        filepath (str): Path to the file containing friendships.
        person1 (str): Name of the first person.
        person2 (str): Name of the second person.

    Returns:
        int: Degree of separation between person1 and person2, or -1 if they are not connected.
    """

    graph = parse_friends_file(filepath)
    matrix, node_index = build_adjacency_matrix(graph)

    return degree_of_separation(matrix, node_index, person1, person2)
