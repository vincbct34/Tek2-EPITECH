##
## EPITECH PROJECT, 2025
## Game of Stones
## File description:
## utilities.py
##

from collections import OrderedDict
import plotly.graph_objects as go
import random
import sys

def display_graph(graph):
    """Display a graph using NetworkX and Matplotlib.

    Args:
        graph (dict): A dictionary representing the graph of friendships.
    """
    
    nodes = list(set(graph.keys()) | {n for sublist in graph.values() for n in sublist})
    positions = {n: (random.uniform(0, 1), random.uniform(0, 1)) for n in nodes}

    edges_x, edges_y = [], []
    for source, targets in graph.items():
        for target in targets:
            x0, y0 = positions[source]
            x1, y1 = positions[target]
            edges_x.extend([x0, x1, None])
            edges_y.extend([y0, y1, None])

    node_x, node_y = zip(*positions.values())

    fig = go.Figure()
    fig.add_trace(go.Scatter(x=edges_x, y=edges_y, mode='lines', line=dict(width=0.5, color='gray')))
    fig.add_trace(go.Scatter(x=node_x, y=node_y, mode='markers+text', text=nodes, marker=dict(size=10, color='blue')))
    fig.show()

def parse_friends_file(filepath):

    """Parse a file containing friendships between people.

    Args:
        filepath (str): Path to the file containing friendships.

    Returns:
        dict: A dictionary representing the graph of friendships.
    """

    graph = {}

    try:
        with open(filepath, 'r') as file:
            if file.read().strip() == "":
                print(f"Error: File '{filepath}' is empty.")
                sys.exit(84)

            file.seek(0)

            for line in file:
                if " is friends with " not in line:
                    print(f"Error: Invalid line in file '{filepath}': {line.strip()}")
                    sys.exit(84)

                person1, person2 = line.strip().split(" is friends with ")

                if person1 not in graph:
                    graph[person1] = []

                if person2 not in graph:
                    graph[person2] = []

                graph[person1].append(person2)
                graph[person2].append(person1)
    except FileNotFoundError:
        print(f"Error: File '{filepath}' not found.")
        sys.exit(84)

    sorted_graph = OrderedDict()

    for person in sorted(graph.keys()):
        graph[person].sort()
        sorted_graph[person] = graph[person]

    return sorted_graph
