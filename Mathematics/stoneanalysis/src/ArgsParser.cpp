/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** ArgsParser
*/

#include "ArgsParser.hpp"

ArgsParser::ArgsParser(int ac, char **av) {
  // Check if there are enough arguments
  if (ac < 2) {
    throw ArgumentsError("No arguments provided. Use --help for usage.");
  }

  std::string opt = av[1];

  // Parse the first argument to determine the mode
  if (opt == "--help") {
    mode = Mode::HELP; // Help mode
  } else if (opt == "--analyze" && ac == 4) {
    mode = Mode::ANALYZE; // Analyze mode
    inputFile = av[2];
    n = std::stoi(av[3]); // Parse the number argument
  } else if (opt == "--cypher" && ac == 5) {
    mode = Mode::CYPHER; // Cypher mode
    inputFile = av[2];
    outputFile = av[3];
    message = av[4]; // Store the message to cypher
  } else if (opt == "--decypher" && ac == 3) {
    mode = Mode::DECYPHER; // Decypher mode
    inputFile = av[2];
  } else {
    // If arguments do not match any mode, throw an error
    throw ArgumentsError("Use --help for usage.");
  }
}
