/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** ArgsParser
*/

#pragma once

#include "Exceptions.hpp"

#include <iostream>
#include <stdexcept>
#include <string>

enum class Mode { HELP, ANALYZE, CYPHER, DECYPHER }; // Enum to represent the mode of operation

class ArgsParser {
public:
  /**
   * @brief Construct a new ArgsParser object
   * @param ac Argument count
   * @param av Argument vector
   */
  ArgsParser(int ac, char **av);

  /**
   * @brief Get the mode of operation
   * @return The mode of operation
   */
  Mode getMode() const { return mode; }

  /**
   * @brief Get the input file name
   * @return The input file name
   */
  const std::string &getInputFile() const { return inputFile; }
  /**
   * @brief Get the output file name
   * @return The output file name
   */
  const std::string &getOutputFile() const { return outputFile; }
  /**
   * @brief Get the message to be cyphered
   * @return The message to be cyphered
   */
  const std::string &getMessage() const { return message; }

  /**
   * @brief Get the number of top frequencies to display
   * @return The number of top frequencies
   */
  int getN() const { return n; }

private:
  Mode mode;

  std::string inputFile; // Input file name
  std::string outputFile; // Output file name
  std::string message; // Message to be cyphered

  int n = 0; // Number of top frequencies to display, default is 0
};
