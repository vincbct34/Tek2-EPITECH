##
## EPITECH PROJECT, 2025
## Game of Stones
## File description:
## main.py
##

from inspect import _void
import argparse
import sys

from src.links import get_degree_of_separation_from_file
from src.plots import analyze_conspiracy_plots

class CustomArgumentParser(argparse.ArgumentParser):
    """Custom ArgumentParser class to handle required return codes.
    """
    def error(self, message):
        """Print the error message and exit with return code 84.
        """
        print(f"Error: {message}", file=sys.stderr)

        sys.exit(84)

def main():
    """Main function of the program, that parses the command line arguments and calls the appropriate functions.
    """

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
        names, relationships, conspiracies, result = analyze_conspiracy_plots(fr, cr, n)

        print("Names:")
        for name in names:
            print(f"{name}")

        print("\nRelationships:")
        for row in relationships:
            print(" ".join(map(str, row)))

        print("\nConspiracies:")
        _void(conspiracies)
        # TODO: Method of print for conspiracies

        print("\nResult:")
        print(f"{'The stone is safe!' if result else 'There is only one way out: treason!'}")
