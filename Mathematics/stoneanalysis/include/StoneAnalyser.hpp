/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** Stone_Analyser
*/

#pragma once

#include "FFT.hpp"
#include "WavFile.hpp"

#include <algorithm>
#include <complex>
#include <iomanip>
#include <iostream>

class StoneAnalyzer {
public:
  /**
   * @brief Construct a new StoneAnalyzer object
   * @param wav The WavFile object to analyze
   * @param sampleRate The sample rate of the audio file
   */
  StoneAnalyzer(const WavFile &wav, double sampleRate);

  /**
   * @brief Analyze the audio file and print the top N frequencies
   * @param topN The number of top frequencies to display
   */
  void analyze(int topN) const;

private:
  const WavFile &wavFile; // The WavFile object

  double sampleRate; // The sample rate of the audio file
};
