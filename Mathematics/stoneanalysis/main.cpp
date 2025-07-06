/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** main
*/

#include "ArgsParser.hpp"
#include "Exceptions.hpp"
#include "Steganographer.hpp"
#include "StoneAnalyser.hpp"
#include "WavFile.hpp"

#include <iostream>

int main(int ac, char **av) {
  try {
    ArgsParser args(ac, av);

    switch (args.getMode()) {
    case Mode::HELP: // Usage message
      std::cout << "USAGE\n"
                << "./stone_analysis [--analyze IN_FILE N | --cypher IN_FILE "
                   "OUT_FILE MESSAGE | --decypher IN_FILE]\n\n"
                << "IN_FILE    An audio file to be analyzed\n"
                << "OUT_FILE   Output audio file of the cypher mode\n"
                << "MESSAGE    The message to hide in the audio file\n"
                << "N          Number of top frequencies to display\n";
      break;

    case Mode::ANALYZE: { // Analyze the audio file
      WavFile wav(args.getInputFile());
      StoneAnalyzer analyzer(wav, SAMPLE_RATE);
      analyzer.analyze(args.getN());
      break;
    }

    case Mode::CYPHER: { // Encode a message in the audio file
      WavFile wav(args.getInputFile());
      Steganographer steg(wav);

      auto samples = steg.encodeMessage(args.getMessage());
      wav.save(args.getOutputFile(), samples);
      break;
    }

    case Mode::DECYPHER: { // Decode a message from the audio file
      WavFile wav(args.getInputFile());
      Steganographer steg(wav);
      std::cout << steg.decodeMessage() << std::endl;
      break;
    }
    }
  } catch (const Exception &e) { // Handle exceptions
    std::cerr << e.what() << std::endl;
    return 84;
  }

  return 0;
}
