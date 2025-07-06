/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** tests_Exceptions
*/

#include "Exceptions.hpp"

#include <criterion/criterion.h>

Test(Exceptions, Exception_message_prefix) {
  Exception e("test");
  cr_assert(std::string(e.what()).find("Stone Analysis: test") !=
            std::string::npos);
}

Test(Exceptions, ArgumentsError_message_prefix) {
  ArgumentsError e("arg");
  cr_assert(std::string(e.what()).find("Arguments Error: arg") !=
            std::string::npos);
}

Test(Exceptions, FeatureError_message_prefix) {
  FeatureError e("feature");
  cr_assert(std::string(e.what()).find("Feature Error: feature") !=
            std::string::npos);
}

Test(Exceptions, Exception_throw_catch) {
  try {
    throw Exception("test");
  } catch (const Exception& e) {
    cr_assert(std::string(e.what()).find("Stone Analysis: test") != std::string::npos);
  }
}

Test(Exceptions, ArgumentsError_throw_catch) {
  try {
    throw ArgumentsError("arg");
  } catch (const ArgumentsError& e) {
    cr_assert(std::string(e.what()).find("Arguments Error: arg") != std::string::npos);
  }
}

Test(Exceptions, FeatureError_throw_catch) {
  try {
    throw FeatureError("feature");
  } catch (const FeatureError& e) {
    cr_assert(std::string(e.what()).find("Feature Error: feature") != std::string::npos);
  }
}
