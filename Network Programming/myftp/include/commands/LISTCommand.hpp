/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** LISTCommand.hpp
*/

#pragma once

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <sstream>

#include "SocketActions.hpp"
#include "FTPCommand.hpp"

class LISTCommand : public FTPCommand {
public:
    std::string execute(const std::string& args, ClientSession& session) override;
};
