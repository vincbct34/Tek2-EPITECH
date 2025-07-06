/*
** EPITECH PROJECT, 2025
** myFTP
** File description:
** PASVCommand.hpp
*/

#pragma once

#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <sstream>

#include "SocketActions.hpp"
#include "FTPCommand.hpp"

class PASVCommand : public FTPCommand {
public:
    std::string execute(const std::string& args, ClientSession& session) override;
};
