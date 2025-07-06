##
## EPITECH PROJECT, 2025
## Game of Stones
## File description:
## main.py
##

import argparse
from os import error
import sys

from src.links import get_degree_of_separation_from_file
from src.plots import analyze_conspiracy_plots

class CustomArgumentParser(argparse.ArgumentParser):

    """Custom ArgumentParser to handle errors.

    Args:
        argparse (ArgumentParser): ArgumentParser class from the argparse module.
    """

    def error(self, message):
        print(f"Error: {message}\n", file=sys.stderr)
        self.print_help()

        sys.exit(84)

def main():

    """Main function of the program, that parses the command line arguments and calls the appropriate functions."""

    parser = CustomArgumentParser(description="Game of Stones CLI tool")

    group = parser.add_mutually_exclusive_group(required=True)

    group.add_argument("--links", nargs=3, metavar=("fr", "p1", "p2"),
                       help="Find links between two people. Requires a friendship file (fr) and two names (p1, p2).")
    group.add_argument("--plots", nargs=3, metavar=("fr", "cr", "n"),
                       help="Analyze conspiracy plots. Requires a friendship file (fr), a conspiracies file (cr), and a maximum path length (n).")

    args = parser.parse_args()

    if args.links:
        fr, p1, p2 = args.links
        degree = get_degree_of_separation_from_file(fr, p1, p2)

        print(f"Degree of separation between {p1} and {p2}: {degree}")

    elif args.plots:
        fr, cr, n = args.plots
        if not n.isdigit() or int(n) <= 0:
            print("Error: n must be a positive integer", file=sys.stderr)
            sys.exit(84)
        names, relationships, conspiracies, result, still_remaining = analyze_conspiracy_plots(fr, cr, int(n))

        print("Names:")
        for name in names:
            print(f"{name}")

        print("\nRelationships:")
        for row in relationships:
            print(" ".join(map(lambda x: '0' if x == float('inf') else str(x), row)))

        print("\nConspiracies:")
        for conspiracy in conspiracies:
            print(" -> ".join(conspiracy))

        if still_remaining:
            for enemy in sorted(still_remaining):
                print(f"No conspiracy possible against {enemy}")

        print("\nResult:")
        print(f"{'The stone is safe!' if result else 'There is only one way out: treason!'}")
