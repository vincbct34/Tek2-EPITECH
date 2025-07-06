/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** FFT
*/

#pragma once

#include "Exceptions.hpp"

#include <complex>
#include <future>
#include <vector>

class FFT {
public:
  /**
   * @brief Construct a new FFT object
   */
  FFT() = default;

  /**
   * @brief Perform the Fast Fourier Transform
   * @param in Input vector of complex numbers
   * @param out Output vector of complex numbers
   * @param depth Depth of recursion for parallel processing
   */
  void Forward(const std::vector<std::complex<double>> &in,
               std::vector<std::complex<double>> &out, int depth = 3);
  /**
   * @brief Perform the Inverse Fast Fourier Transform
   * @param in Input vector of complex numbers
   * @param out Output vector of complex numbers
   * @param depth Depth of recursion for parallel processing
   */
  void Inverse(const std::vector<std::complex<double>> &in,
               std::vector<std::complex<double>> &out, int depth = 3);

  /**
   * @brief Get the next power of two greater than or equal to n (used for FFT)
   * @param n The input number
   * @return The next power of two
   */
  size_t nextPowerOfTwo(size_t n);
};
