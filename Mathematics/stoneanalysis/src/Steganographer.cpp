/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** Steganographer
*/

#include "Steganographer.hpp"

Steganographer::Steganographer(WavFile &wav) : wavFile(wav) {}

std::vector<short> Steganographer::encodeMessage(const std::string &message) {
  std::vector<short> samples = wavFile.getSamples();
  // Calculate total bits needed (including null terminator)
  const size_t totalBits = (message.size() + 1) * 8;

  if (samples.size() < totalBits)
    throw FeatureError("Audio file is too small to encode the full message.");

  size_t index = 0;

  // Encode each character bit by bit into the LSB of each sample
  for (size_t i = 0; i <= message.size(); ++i) {
    unsigned char c = (i < message.size()) ? message[i] : '\0';

    for (int bit = 0; bit < 8; ++bit) {
      short &s = samples[index++];
      s = (s & ~1) | ((c >> bit) & 1); // Set LSB to message bit
    }
  }

  return samples;
}

std::string Steganographer::decodeMessage() const {
  const std::vector<short> &samples = wavFile.getSamples();
  std::string result;

  size_t index = 0;

  // Read 8 bits at a time to reconstruct each character
  while (index + 8 <= samples.size()) {
    unsigned char c = 0;

    for (int bit = 0; bit < 8; ++bit)
      c |= (samples[index++] & 1) << bit; // Read LSB and shift into char

    if (c == '\0')
      break; // Stop at null terminator

    result.push_back(c);
  }

  return result;
}
