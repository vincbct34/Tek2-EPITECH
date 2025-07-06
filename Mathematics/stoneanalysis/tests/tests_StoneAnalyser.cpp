/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** tests_StoneAnalyser
*/

#include "StoneAnalyser.hpp"
#include "WavFile.hpp"
#include "Exceptions.hpp"
#include <criterion/criterion.h>
#include <string>
#include <unistd.h>

Test(StoneAnalyzer, analyze_top_frequencies) {
    std::string wavPath = "../dummy_stone.wav";
    if (access(wavPath.c_str(), F_OK) != 0) {
        // Create a dummy file with enough samples
        std::vector<short> samples(128, 0x1234);
        SF_INFO sfinfo{};
        sfinfo.samplerate = 48000;
        sfinfo.channels = 1;
        sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
        SNDFILE *outfile = sf_open(wavPath.c_str(), SFM_WRITE, &sfinfo);
        if (outfile) {
            sf_write_short(outfile, samples.data(), samples.size());
            sf_close(outfile);
        }
    }
    WavFile wav(wavPath);
    StoneAnalyzer analyzer(wav, 48000.0);
    // Should not throw for valid file
    analyzer.analyze(5);
}

Test(StoneAnalyzer, not_enough_samples_throws) {
    std::string wavPath = "../dummy_stone_few.wav";
    if (access(wavPath.c_str(), F_OK) != 0) {
        // Create a dummy file with very few samples
        std::vector<short> samples(8, 0x1234);
        SF_INFO sfinfo{};
        sfinfo.samplerate = 48000;
        sfinfo.channels = 1;
        sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
        SNDFILE *outfile = sf_open(wavPath.c_str(), SFM_WRITE, &sfinfo);
        if (outfile) {
            sf_write_short(outfile, samples.data(), samples.size());
            sf_close(outfile);
        }
    }
    WavFile wav(wavPath);
    StoneAnalyzer analyzer(wav, 48000.0);
    cr_assert_throw(analyzer.analyze(5), ArgumentsError);
}
