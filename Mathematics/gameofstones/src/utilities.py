##
## EPITECH PROJECT, 2025
## Game of Stones
## File description:
## utilities.py
##

import sys

def parse_conspiracies_file(filepath, friends_graph):

    """Parse a file containing conspiracies between people.

    Args:
        filepath (str): Path to the file containing conspiracies.
        friends_graph (dict): A dictionary representing the graph of friendships.

    Returns:
        dict: A dictionary representing the graph of conspiracies.
    """

    graph = {}

    try:
        with open(filepath, 'r') as file:
            for line in file:
                if " is plotting against " not in line:
                    print(f"Error: Invalid line in file '{filepath}': {line.strip()}", file=sys.stderr)
                    sys.exit(84)

                plotter, target = line.strip().split(" is plotting against ")

                if plotter not in friends_graph or target not in friends_graph:
                    print(f"Error: {plotter} or {target} is in conspiracies but not in friendships!", file=sys.stderr)
                    sys.exit(84)

                if plotter not in graph:
                    graph[plotter] = []

                graph[plotter].append(target)

    except FileNotFoundError:
        print(f"Error: File '{filepath}' not found.", file=sys.stderr)
        sys.exit(84)

    return {k: sorted(v) for k, v in sorted(graph.items())}


from collections import defaultdict

def parse_friends_file(filepath):

    """Optimized function to parse friendships file.

    Args:
        filepath (str): Path to the file containing friendships.

    Returns:
        dict: A dictionary representing the graph of friendships.
    """

    graph = defaultdict(set)

    try:
        with open(filepath, 'r') as file:
            for line in file:
                if " is friends with " not in line:
                    print(f"Error: Invalid line in '{filepath}': {line.strip()}", file=sys.stderr)
                    sys.exit(84)

                person1, person2 = line.strip().split(" is friends with ")

                graph[person1].add(person2)
                graph[person2].add(person1)

    except FileNotFoundError:
        print(f"Error: File '{filepath}' not found.", file=sys.stderr)
        sys.exit(84)

    if "Cersei Lannister" not in graph:
        print("Error: Cersei Lannister is not in the friendships!", file=sys.stderr)
        sys.exit(84)

    return {k: sorted(v) for k, v in sorted(graph.items())}


def build_adjacency_matrix(graph):

    """Optimized adjacency matrix construction.
    
    Args:
        graph (dict): A dictionary representing the graph of friendships.
        
    Returns:
        list: Adjacency matrix representing the graph.
        dict: Dictionary mapping person names to their index in the matrix.
    """

    nodes = sorted(graph.keys())
    node_index = {node: idx for idx, node in enumerate(nodes)}

    size = len(nodes)
    matrix = [[0] * size for _ in range(size)]

    for node, friends in graph.items():
        i = node_index[node]
        for friend in friends:
            j = node_index[friend]
            matrix[i][j] = 1

    return matrix, node_index
