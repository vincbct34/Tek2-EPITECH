/*
** EPITECH PROJECT, 2025
** StoneAnalysis [WSL: Ubuntu-24.04]
** File description:
** WavFile
*/

#include "WavFile.hpp"

WavFile::WavFile(const std::string &path) {
  SF_INFO sfinfo{};
  // Open the WAV file for reading
  SNDFILE *infile = sf_open(path.c_str(), SFM_READ, &sfinfo);

  if (!infile)
    throw ArgumentsError("Failed to open input file: " + path);

  // Validate WAV file properties
  if (sfinfo.samplerate != 48000)
    throw ArgumentsError("WAV file does not have 48kHz sample rate: " + path);
  if (sfinfo.channels != 1)
    throw ArgumentsError("WAV file is not mono (1 channel): " + path);
  if ((sfinfo.format & SF_FORMAT_TYPEMASK) != SF_FORMAT_WAV)
    throw ArgumentsError("WAV file is not in WAV format: " + path);
  if ((sfinfo.format & SF_FORMAT_SUBMASK) != SF_FORMAT_PCM_16)
    throw ArgumentsError("WAV file is not 16-bit PCM: " + path);

  // Read samples from the file
  samples.resize(sfinfo.frames);
  sf_count_t count = sf_read_short(infile, samples.data(), sfinfo.frames);

  if (count != sfinfo.frames)
    throw ArgumentsError("Error reading WAV data from: " + path);

  sf_close(infile);
}

void WavFile::save(const std::string &path,
                   const std::vector<short> &newSamples) const {
  SF_INFO sfinfo{};

  // Set WAV file properties for saving
  sfinfo.samplerate = 48000;
  sfinfo.channels = 1;
  sfinfo.format = SF_FORMAT_WAV | SF_FORMAT_PCM_16;

  // Open the file for writing
  SNDFILE *outfile = sf_open(path.c_str(), SFM_WRITE, &sfinfo);

  if (!outfile)
    throw ArgumentsError("Failed to write to output file: " + path);

  // Write samples to the file
  sf_count_t count =
      sf_write_short(outfile, newSamples.data(), newSamples.size());

  if (count != static_cast<sf_count_t>(newSamples.size()))
    throw ArgumentsError("Failed to write all samples to output file: " + path);

  sf_close(outfile);
}
