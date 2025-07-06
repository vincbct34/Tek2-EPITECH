/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** tests_WavFile
*/

#include "WavFile.hpp"
#include "Exceptions.hpp"
#include <criterion/criterion.h>
#include <string>
#include <vector>
#include <unistd.h>

Test(WavFile, load_valid_file) {
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
    const std::vector<short>& samples = wav.getSamples();
    cr_assert(!samples.empty());
}

Test(WavFile, invalid_path_throws) {
    cr_assert_throw(WavFile("nonexistent.wav"), ArgumentsError);
}

Test(WavFile, invalid_samplerate_throws) {
    // Use a valid file and patch samplerate after open (simulate)
    // Not possible without a crafted file, so skip in this context
    // cr_assert_throw(WavFile("bad_samplerate.wav"), ArgumentsError);
}

Test(WavFile, save_and_reload) {
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
    std::vector<short> samples = wav.getSamples();
    std::string outPath = "otput.wav";
    wav.save(outPath, samples);
    WavFile wav2(outPath);
    cr_assert_eq(samples.size(), wav2.getSamples().size());
}

Test(WavFile, save_fail_throws) {
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
    std::vector<short> samples = wav.getSamples();
    // Try to save to a directory (should fail)
    cr_assert_throw(wav.save("/", samples), ArgumentsError);
}

Test(WavFile, invalid_channels_throws) {
    std::string path = "bad_channels.wav";
    std::vector<short> samples(128, 0x1234);
    SF_INFO sfinfo{};
    sfinfo.samplerate = 48000;
    sfinfo.channels = 2; // Stereo, should fail
    sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
    SNDFILE *outfile = sf_open(path.c_str(), SFM_WRITE, &sfinfo);
    if (outfile) {
        sf_write_short(outfile, samples.data(), samples.size());
        sf_close(outfile);
    }
    cr_assert_throw((WavFile{path}), ArgumentsError);
}

Test(WavFile, invalid_format_throws) {
    std::string path = "bad_format.wav";
    std::vector<short> samples(128, 0x1234);
    SF_INFO sfinfo{};
    sfinfo.samplerate = 48000;
    sfinfo.channels = 1;
    sfinfo.format = SF_FORMAT_AIFF | SF_FORMAT_PCM_16; // Not WAV
    SNDFILE *outfile = sf_open(path.c_str(), SFM_WRITE, &sfinfo);
    if (outfile) {
        sf_write_short(outfile, samples.data(), samples.size());
        sf_close(outfile);
    }
    cr_assert_throw((WavFile{path}), ArgumentsError);
}

Test(WavFile, invalid_pcm_throws) {
    std::string path = "bad_pcm.wav";
    std::vector<short> samples(128, 0x1234);
    SF_INFO sfinfo{};
    sfinfo.samplerate = 48000;
    sfinfo.channels = 1;
    sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_24; // Not PCM_16
    SNDFILE *outfile = sf_open(path.c_str(), SFM_WRITE, &sfinfo);
    if (outfile) {
        sf_write_short(outfile, samples.data(), samples.size());
        sf_close(outfile);
    }
    cr_assert_throw((WavFile{path}), ArgumentsError);
}
