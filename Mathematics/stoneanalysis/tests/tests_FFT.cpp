/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** tests_FFT
*/

#include "FFT.hpp"

#include <criterion/criterion.h>

Test(FFT, forward_and_inverse_identity) {
  FFT fft;
  std::vector<std::complex<double>> input = {1, 2, 3, 4};
  std::vector<std::complex<double>> output;
  std::vector<std::complex<double>> inverse;

  fft.Forward(input, output);
  fft.Inverse(output, inverse);
  cr_assert_eq(input.size(), inverse.size());
  for (size_t i = 0; i < input.size(); ++i) {
    cr_assert_float_eq(input[i].real(), inverse[i].real(), 1e-9);
    cr_assert_float_eq(input[i].imag(), inverse[i].imag(), 1e-9);
  }
}

Test(FFT, forward_power_of_two_check) {
  FFT fft;
  std::vector<std::complex<double>> input = {1, 2, 3, 4};
  std::vector<std::complex<double>> output;
  fft.Forward(input, output);
}

Test(FFT, forward_non_power_of_two_throws) {
  FFT fft;
  std::vector<std::complex<double>> input = {1, 2, 3};
  std::vector<std::complex<double>> output;
  cr_assert_throw(fft.Forward(input, output), ArgumentsError);
}

Test(FFT, next_power_of_two) {
  FFT fft;
  cr_assert_eq(fft.nextPowerOfTwo(0), 1UL);
  cr_assert_eq(fft.nextPowerOfTwo(1), 1UL);
  cr_assert_eq(fft.nextPowerOfTwo(2), 2UL);
  cr_assert_eq(fft.nextPowerOfTwo(3), 4UL);
  cr_assert_eq(fft.nextPowerOfTwo(5), 8UL);
  cr_assert_eq(fft.nextPowerOfTwo(16), 16UL);
}

Test(FFT, forward_empty_input) {
  FFT fft;
  std::vector<std::complex<double>> input;
  std::vector<std::complex<double>> output;
  fft.Forward(input, output);
  cr_assert(output.empty());
}

Test(FFT, inverse_empty_input) {
  FFT fft;
  std::vector<std::complex<double>> input;
  std::vector<std::complex<double>> output;
  fft.Inverse(input, output);
  cr_assert(output.empty());
}

Test(FFT, forward_parallel_branch) {
  FFT fft;
  std::vector<std::complex<double>> input(8, 1.0);
  std::vector<std::complex<double>> output;
  fft.Forward(input, output, 1); // depth > 0 triggers parallel branch
  cr_assert_eq(output.size(), input.size());
}
