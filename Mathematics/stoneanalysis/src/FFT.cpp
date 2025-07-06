/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** FFT
*/

#include "FFT.hpp"

void FFT::Forward(const std::vector<std::complex<double>> &in,
                  std::vector<std::complex<double>> &out, int depth) {
  size_t n = in.size();
  if (n == 0)
    return;

  out.resize(n);

  // Base case: single element
  if (n == 1) {
    out[0] = in[0];
    return;
  }

  // Ensure input size is a power of 2
  if ((n & (n - 1)) != 0)
    throw ArgumentsError(
        "FFT input size must be a power of 2, check your input file.");

  // Split input into even and odd indices
  std::vector<std::complex<double>> even(n / 2), odd(n / 2);
  for (size_t i = 0; i < n / 2; ++i) {
    even[i] = in[i * 2];
    odd[i] = in[i * 2 + 1];
  }

  std::vector<std::complex<double>> even_fft(n / 2), odd_fft(n / 2);

  // Recursive FFT calls, possibly in parallel
  if (depth > 0) {
    auto future_even = std::async(
        std::launch::async, [&]() { Forward(even, even_fft, depth - 1); });

    Forward(odd, odd_fft, depth - 1);
    future_even.wait();
  } else {
    Forward(even, even_fft, 0);
    Forward(odd, odd_fft, 0);
  }

  // Combine results using twiddle factors
  for (size_t k = 0; k < n / 2; ++k) {
    std::complex<double> twiddle = std::polar(1.0, -2.0 * M_PI * k / n);
    out[k] = even_fft[k] + twiddle * odd_fft[k];
    out[k + n / 2] = even_fft[k] - twiddle * odd_fft[k];
  }
}

void FFT::Inverse(const std::vector<std::complex<double>> &in,
                  std::vector<std::complex<double>> &out, int depth) {
  size_t n = in.size();

  if (n == 0)
    return;

  // Take the conjugate of the input
  std::vector<std::complex<double>> conjugated(n);
  for (size_t i = 0; i < n; ++i)
    conjugated[i] = std::conj(in[i]);

  std::vector<std::complex<double>> temp(n);

  // Perform forward FFT on conjugated input
  Forward(conjugated, temp, depth);
  out.resize(n);
  for (size_t i = 0; i < n; ++i)
    // Take conjugate again and normalize
    out[i] = std::conj(temp[i]) / static_cast<double>(n);
}

size_t FFT::nextPowerOfTwo(size_t n) {
  if (n == 0)
    return 1;

  size_t power = 1;
  // Find the next power of two greater than or equal to n
  while (power < n)
    power <<= 1;

  return power;
}
