##
## EPITECH PROJECT, 2025
## Game of Stones
## File description:
## links.py
##

from src.utilities import parse_friends_file, display_graph

def build_adjacency_matrix(graph):

    """Build an adjacency matrix from a graph.

    Args:
        graph (dict): A dictionary representing the graph of friendships.

    Returns:
        tuple: A tuple containing the adjacency matrix and a dictionary mapping nodes to indices.
    """

    nodes = list(graph.keys())
    size = len(nodes)

    matrix = [[0] * size for _ in range(size)]
    node_index = {node: idx for idx, node in enumerate(nodes)}

    for node, friends in graph.items():
        for friend in friends:
            i, j = node_index[node], node_index[friend]
            matrix[i][j] = 1
            matrix[j][i] = 1

    return matrix, node_index

def degree_of_separation(matrix, node_index, person1, person2):

    """Calculate the degree of separation between two people.

    Args:
        matrix (list): Adjacency matrix of the graph.
        node_index (dict): Dictionary mapping nodes to indices.
        person1 (str): Name of the first person.
        person2 (str): Name of the second person.

    Returns:
        int: Degree of separation between person1 and person2, or -1 if they are not connected.
    """

    if person1 not in node_index or person2 not in node_index:
        return -1

    start, end = node_index[person1], node_index[person2]
    visited = [False] * len(matrix)
    queue = [(start, 0)]

    while queue:
        current, degree = queue.pop(0)

        if current == end:
            return degree

        for neighbor, connected in enumerate(matrix[current]):

            if connected and not visited[neighbor]:
                visited[neighbor] = True
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

    display_graph(graph)

    return degree_of_separation(matrix, node_index, person1, person2)
