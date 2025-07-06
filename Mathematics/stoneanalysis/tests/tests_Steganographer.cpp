/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** tests_Steganographer
*/

#define UNIT_TEST_FRIEND

#include "Steganographer.hpp"
#include "WavFile.hpp"
#include "Exceptions.hpp"
#include <criterion/criterion.h>
#include <vector>
#include <string>
#include <unistd.h>

Test(Steganographer, encode_and_decode_message) {
    // Prepare a dummy wav file with enough samples
    std::string wavPath = "../dummy_stone.wav";
    if (access(wavPath.c_str(), F_OK) != 0) {
        // Create a dummy file with enough samples (at least 64 samples for "Hello!\0")
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
    Steganographer steg(wav);
    std::string message = "Hello!";
    std::vector<short> encoded = steg.encodeMessage(message);
    // Save encoded samples to a temp file
    std::string outPath = "otput.wav";
    wav.save(outPath, encoded);
    // Reload and decode
    WavFile wav2(outPath);
    Steganographer steg2(wav2);
    std::string decoded = steg2.decodeMessage();
    cr_assert_eq(message, decoded);
}

Test(Steganographer, encode_too_large_message_throws) {
    std::string wavPath = "../dummy_stone_few.wav";
    if (access(wavPath.c_str(), F_OK) != 0) {
        // Create a dummy file with very few samples (less than 8*1000)
        std::vector<short> samples(16, 0x1234);
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
    Steganographer steg(wav);
    std::string message(1000, 'A'); // Too large for dummy_stone_few.wav
    cr_assert_throw(steg.encodeMessage(message), FeatureError);
}

Test(Steganographer, decode_empty_message) {
    std::vector<short> samples(8, 0); // Only null terminator
    std::string path = "empty_message.wav";
    SF_INFO sfinfo{};
    sfinfo.samplerate = 48000;
    sfinfo.channels = 1;
    sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
    SNDFILE *outfile = sf_open(path.c_str(), SFM_WRITE, &sfinfo);
    if (outfile) {
        sf_write_short(outfile, samples.data(), samples.size());
        sf_close(outfile);
    }
    WavFile wav(path);
    Steganographer steg(wav);
    std::string decoded = steg.decodeMessage();
    cr_assert(decoded.empty());
}

Test(Steganographer, encode_and_decode_empty_string) {
    std::vector<short> samples(8, 0x1234);
    std::string path = "empty_string.wav";
    SF_INFO sfinfo{};
    sfinfo.samplerate = 48000;
    sfinfo.channels = 1;
    sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
    SNDFILE *outfile = sf_open(path.c_str(), SFM_WRITE, &sfinfo);
    if (outfile) {
        sf_write_short(outfile, samples.data(), samples.size());
        sf_close(outfile);
    }
    WavFile wav(path);
    Steganographer steg(wav);
    std::vector<short> encoded = steg.encodeMessage("");
    wav.save("empty_string_out.wav", encoded);
    WavFile wav2("empty_string_out.wav");
    Steganographer steg2(wav2);
    std::string decoded = steg2.decodeMessage();
    cr_assert(decoded.empty());
}
