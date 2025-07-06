/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** WavFile
*/

#pragma once

#include "Exceptions.hpp"

#include <cstdint>
#include <fstream>
#include <iostream>
#include <sndfile.h>
#include <stdexcept>
#include <string>
#include <vector>

#define SAMPLE_RATE 48000.0 // The sample rate given in the subject

/**
 * @brief Class to handle WAV files
 */
class WavFile {
public:
  /**
   * @brief Construct a new WavFile object
   * @param path The path to the WAV file
   */
  explicit WavFile(const std::string &path);

  /**
   * @brief Save the WAV file with new samples
   * @param path The path to save the WAV file
   * @param newSamples The new samples to save
   */
  void save(const std::string &path,
            const std::vector<short> &newSamples) const;

  /**
   * @brief Get the samples of the WAV file
   * @return The samples of the WAV file
   */
  const std::vector<short> &getSamples() const { return samples; }

private:
  std::vector<short> samples; // Audio samples
};
