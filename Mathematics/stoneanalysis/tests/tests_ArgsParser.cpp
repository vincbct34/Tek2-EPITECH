/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** tests_ArgsParser
*/

#include "ArgsParser.hpp"

#include <criterion/criterion.h>

Test(ArgsParser, help_mode) {
  const char *argv[] = {"prog", "--help"};
  ArgsParser parser(2, const_cast<char **>(argv));
  cr_assert_eq(parser.getMode(), Mode::HELP);
}

Test(ArgsParser, analyze_mode) {
  const char *argv[] = {"prog", "--analyze", "input.wav", "5"};
  ArgsParser parser(4, const_cast<char **>(argv));
  cr_assert_eq(parser.getMode(), Mode::ANALYZE);
  cr_assert_eq(parser.getInputFile(), std::string("input.wav"));
  cr_assert_eq(parser.getN(), 5);
}

Test(ArgsParser, cypher_mode) {
  const char *argv[] = {"prog", "--cypher", "in.wav", "out.wav", "msg"};
  ArgsParser parser(5, const_cast<char **>(argv));
  cr_assert_eq(parser.getMode(), Mode::CYPHER);
  cr_assert_eq(parser.getInputFile(), std::string("in.wav"));
  cr_assert_eq(parser.getOutputFile(), std::string("out.wav"));
  cr_assert_eq(parser.getMessage(), std::string("msg"));
}

Test(ArgsParser, decypher_mode) {
  const char *argv[] = {"prog", "--decypher", "in.wav"};
  ArgsParser parser(3, const_cast<char **>(argv));
  cr_assert_eq(parser.getMode(), Mode::DECYPHER);
  cr_assert_eq(parser.getInputFile(), std::string("in.wav"));
}

Test(ArgsParser, invalid_args_throws) {
  const char *argv[] = {"prog", "--invalid"};
  cr_assert_throw(ArgsParser(2, const_cast<char **>(argv)), ArgumentsError);
}

Test(ArgsParser, not_enough_args_throws) {
  const char *argv[] = {"prog"};
  cr_assert_throw(ArgsParser(1, const_cast<char **>(argv)), ArgumentsError);
}

Test(ArgsParser, analyze_wrong_argc_throws) {
  const char *argv[] = {"prog", "--analyze", "input.wav"};
  cr_assert_throw(ArgsParser(3, const_cast<char **>(argv)), ArgumentsError);
}

Test(ArgsParser, cypher_wrong_argc_throws) {
  const char *argv[] = {"prog", "--cypher", "in.wav", "out.wav"};
  cr_assert_throw(ArgsParser(4, const_cast<char **>(argv)), ArgumentsError);
}

Test(ArgsParser, decypher_wrong_argc_throws) {
  const char *argv[] = {"prog", "--decypher"};
  cr_assert_throw(ArgsParser(2, const_cast<char **>(argv)), ArgumentsError);
}
