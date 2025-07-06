/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** Steganographer
*/

#pragma once

#include "Exceptions.hpp"
#include "FFT.hpp"
#include "WavFile.hpp"

#include <bitset>
#include <cmath>
#include <complex>
#include <memory>
#include <sndfile.h>
#include <stdexcept>
#include <string>
#include <unordered_map>

class Steganographer {
public:
  /**
   * @brief Construct a new Steganographer object
   * @param wav The WavFile object to be used for encoding/decoding
   */
  explicit Steganographer(WavFile &wav);

  /**
   * @brief Encode a message into the audio file
   * @param message The message to be encoded
   */
  std::vector<short> encodeMessage(const std::string &message);

  /**
   * @brief Decode a message from the audio file
   * @return The decoded message
   */
  std::string decodeMessage() const;

private:
  WavFile &wavFile; // Reference to the WavFile object

// For test coverage: allow test access to private encodeChar and decodeChar
#ifdef UNIT_TEST_FRIEND
  friend class FriendStegTest;
#endif
};
