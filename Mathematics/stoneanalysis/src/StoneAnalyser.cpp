/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** StoneAnalyser
*/

#include "StoneAnalyser.hpp"

StoneAnalyzer::StoneAnalyzer(const WavFile &wav, double sampleRate)
    : wavFile(wav), sampleRate(sampleRate) {}

void StoneAnalyzer::analyze(int topN) const {
  const auto &samples = wavFile.getSamples();

  // Check if there are enough samples for analysis
  if (samples.size() < 16)
    throw ArgumentsError("Not enough samples to analyze.");

  // Convert samples to complex numbers for FFT
  std::vector<std::complex<double>> inputComplex(samples.begin(),
                                                 samples.end());

  FFT fft;
  // Pad input to next power of two for FFT
  size_t paddedSize = fft.nextPowerOfTwo(inputComplex.size());
  inputComplex.resize(paddedSize, 0.0);

  std::vector<std::complex<double>> outputComplex;

  // Perform FFT
  fft.Forward(inputComplex, outputComplex);

  std::vector<std::pair<double, int>> magnitudes;

  // Calculate magnitude for each frequency bin
  for (size_t k = 0; k < outputComplex.size() / 2; ++k) {
    double mag = std::abs(outputComplex[k]);
    magnitudes.emplace_back(mag, k);
  }

  // Sort frequencies by magnitude (descending)
  std::sort(magnitudes.begin(), magnitudes.end(),
            [](const auto &a, const auto &b) { return a.first > b.first; });

  std::cout << std::fixed << std::setprecision(1);
  std::cout << "Top " << topN << " frequencies:\n";
  // Output the top N frequencies
  for (int i = 0; i < topN && i < static_cast<int>(magnitudes.size()); ++i) {
    double frequency = (magnitudes[i].second * sampleRate) / paddedSize;
    std::cout << frequency << " Hz\n";
  }
}
