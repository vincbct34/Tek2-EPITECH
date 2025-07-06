/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** DELECommand.hpp
*/

#pragma once

#include <filesystem>
#include <unistd.h>

#include "FTPCommand.hpp"

class DELECommand : public FTPCommand {
public:
    std::string execute(const std::string& args, ClientSession& session) override;
};
